# Checking data quality of service use measures - distribution by practice and month

from cohortextractor import (
    StudyDefinition, 
    Measure, 
    patients)

## CODELISTS ##

# Import codelists from the codelist folder

from codelists import *

## STUDY DEFINITION ##

study = StudyDefinition(
    # Configure the expectations framework
    default_expectations={
        "date": {"earliest": "2019-03-01", "latest": "2021-02-28"},
        "rate": "exponential_increase",
        "incidence": 0.2,
    },

    index_date="2019-03-01",

    population=patients.registered_as_of("index_date"),

    practice_id =patients.registered_practice_as_of(
        "index_date",
        returning="pseudo_id",
        return_expectations={
            "int": {"distribution": "normal", "mean": 2000, "stddev": 250},
            "incidence": 1
        }
    ),

    aevis = patients.attended_emergency_care(
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    adm = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
        }
    ),

    emadm = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    eladm = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    beddays = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    beddays_emadm = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    beddays_eladm = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_beddays = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_beddays_emadm = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_beddays_eladm = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    opapp = patients.outpatient_appointment_date(
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    opatt = patients.outpatient_appointment_date(
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        attended = True,
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## GP clinical coded activity in year prior to death
    # Use as a proxy for contact with GP
    
    gp = patients.with_gp_consultations(
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## EOL medication
    # Start with midazolam as existing codelist
    eol_med = patients.with_these_medications(
        midazolam_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    )
)

measures = [
    Measure(
        id="aevis_by_practice",
        numerator="aevis",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="adm_by_practice",
        numerator="adm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="emadm_by_practice",
        numerator="emadm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="eladm_by_practice",
        numerator="eladm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="beddays_by_practice",
        numerator="beddays",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="beddays_emadm_by_practice",
        numerator="beddays_emadm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="beddays_eladm_by_practice",
        numerator="beddays_eladm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="crit_beddays_by_practice",
        numerator="crit_beddays",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="crit_beddays_emadm_by_practice",
        numerator="crit_beddays_emadm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="crit_beddays_eladm_by_practice",
        numerator="crit_beddays_eladm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="opapp_by_practice",
        numerator="opapp",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="opatt_by_practice",
        numerator="opatt",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="gp_by_practice",
        numerator="gp",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="eol_med_by_practice",
        numerator="eol_med",
        denominator="population",
        group_by="practice_id",
    )
]