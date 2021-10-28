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
        },
    ),

    # Clinical

    # Activity

    admission=patients.admitted_to_hospital(
        returning="binary_flag",
        between=["died_date_ons - 1 year", "died_date_ons"],
        find_first_match_in_period=True,
        return_expectations={"date": {"earliest": "2018-03-01"}},
    ),

    emergency_care=patients.attended_emergency_care(
        returning="binary_flag",
        between=["died_date_ons - 1 year", "died_date_ons"],
        find_first_match_in_period=True,
        return_expectations={"date": {"earliest": "2018-03-01"}},
    ),
)

# How to just keep people who died in period of interest?
# Reference date for demographics - death date?
