from cohortextractor import (
    StudyDefinition,
    patients)

## CODELISTS
# Import codelists from the codelist/ folder

from codelists import *

## KEY VARIABLES
EARLIEST="2019-03-01"
LATEST="2021-02-28"

## STUDY POPULATION

study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1970-01-01", "latest": LATEST},
        "rate": "uniform",
        "incidence": 0.5,
    },

    # Study population

    # population=patients.registered_as_of(
    #    "died_date_ons - 1 year"
    # ),

    population=patients.died_from_any_cause(
        between=[EARLIEST, LATEST],
        return_expectations={"incidence": 1.0},
    ),

    # Cohort

    died_date_ons=patients.died_from_any_cause(
        between=[EARLIEST, LATEST],
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": EARLIEST},
            "rate": "uniform"
        },
    ),

    # Demographics

    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        },
    ),

    age=patients.age_as_of(
        "died_date_ons",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),

    ethnicity_16=patients.with_these_clinical_events(
        ethnicity_codes_16,
        returning="category",
        find_last_match_in_period=True,
        return_expectations={
            "category": {"ratios": {"1": 0.8, "5": 0.1, "3": 0.1}},
            "incidence": 0.75,
        },
    ),

    imd=patients.address_as_of(
        "died_date_ons",
        returning="index_of_multiple_deprivation",
        round_to_nearest=100,
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"100": 0.1, "200": 0.2, "300": 0.7}},
        },
    ),

    carehome=patients.care_home_status_as_of(
        "died_date_ons"
    ),

    prison=patients.household_as_of(
        "2020-02-01",
        returning="is_prison"
    ),

    # household_size=patients.household_as_of(
    #    "2020-02-01",
    #    returning="household_size"
    # ),

    # Geography
    msoa=patients.household_as_of(
        "2020-02-01",
        returning="msoa",
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"E02000001": 0.5, "E02000002": 0.5}},
        }
    ),

    # Long term conditions

    ltc=patients.with_these_clinical_events(
        ltc_codes,
        between=["died_date_ons - 1 year", "died_date_ons"],
        returning="binary_flag",
        find_first_match_in_period=True
    ),

    physical_ltc=patients.with_these_clinical_events(
        physical_ltc_codes,
        between=["died_date_ons - 1 year", "died_date_ons"],
        returning="binary_flag",
        find_first_match_in_period=True
    ),

    mental_ltc=patients.with_these_clinical_events(
        mental_ltc_codes,
        between=["died_date_ons - 1 year", "died_date_ons"],
        returning="binary_flag",
        find_first_match_in_period=True
    ),

    # Hospital activity in year prior to death

    ae_visits_1yr=patients.attended_emergency_care(
        returning="number_of_matches_in_period",
        between=["died_date_ons - 1 year", "died_date_ons"],
        return_expectations={"int": {"distribution": "normal", "mean": 5, "stddev": 1}, "incidence": 0.8}
    ),

    admissions_1yr=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["died_date_ons - 1 year", "died_date_ons"],
        return_expectations={"int": {"distribution": "normal", "mean": 5, "stddev": 1}, "incidence": 0.8}
    ),

    emergency_admissions_1yr=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["died_date_ons - 1 year", "died_date_ons"],
        with_admission_method=['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations={"int": {"distribution": "normal", "mean": 5, "stddev": 1}, "incidence": 0.8}
    ),

    elective_admissions_1yr=patients.admitted_to_hospital(
        returning="number_of_matches_in_period",
        between=["died_date_ons - 1 year", "died_date_ons"],
        with_admission_method=['11', '12', '13'],
        return_expectations={"int": {"distribution": "normal", "mean": 5, "stddev": 1}, "incidence": 0.8}
    ),

    #outpatient_appointments_1yr=patients.outpatient_appointment_date(
    #    returning="number_of_matches_in_period",
    #    between=["died_date_ons - 1 year", "died_date_ons"],
    #    attended=True,
    #    return_expectations={"int": {"distribution": "normal", "mean": 5, "stddev": 1}, "incidence": 0.8}
    #),

    #outpatient_attended_1yr=patients.outpatient_appointment_date(
    #    returning="number_of_matches_in_period",
    #    between=["died_date_ons - 1 year", "died_date_ons"],
    #    attended=True,
    #    return_expectations={"int": {"distribution": "normal", "mean": 5, "stddev": 1}, "incidence": 0.8}
    #),

    # Clinically coded activity in year prior to death

    gp_contact_1yr=patients.with_gp_consultations(
        returning="number_of_matches_in_period",
        between=["died_date_ons - 1 year", "died_date_ons"],
        return_expectations={"int": {"distribution": "normal", "mean": 5, "stddev": 1}, "incidence": 0.8}
    ),
    # Not sure if this is the best way to do this

    # gp_ooh_1yr=patients.with_these_clinical_events(
    #    codelist=nhs111,
    #    returning="number_of_matches_in_period",
    #    between=["died_date_ons - 1 year", "died_date_ons"],
    # )

)

# How to just keep people who died in period of interest?
# Reference date for demographics - death date?
