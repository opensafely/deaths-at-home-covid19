# Checking data quality of service use measures - distribution by practice and month

from cohortextractor import (
    StudyDefinition, 
    Measure, 
    patients)

## CODELISTS ##

# Import codelists from the codelist folder

from codelists import *

## LOOP FUNCTIONS ##
# To loop through and find matches for individual snomed codes within a codelist
# Will potentially need to update period to look in

#def make_variable(code):
#    return {
#        f"snomed_{code}": (
#            patients.with_these_clinical_events(
#                codelist([code], system="snomed"),
#                between = ["dod_ons - 365 days", "dod_ons"],
#                returning="number_of_matches_in_period",
#                return_expectations={
#                    "incidence": 0.1,
#                    "int": {"distribution": "normal", "mean": 3, "stddev": 1},
#                },
#            )
#        )
#    }

#def loop_over_codes(code_list):
#    variables = {}
#    for code in code_list:
#        variables.update(make_variable(code))
#    return variables

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

    aevis_binary = patients.attended_emergency_care(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    aevis = patients.attended_emergency_care(
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    adm_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    adm = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
        }
    ),

    emadm_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D']
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

    eladm_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['11', '12', '13']
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

    beddays_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    beddays = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    beddays_emadm_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D']
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

    beddays_eladm_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['11', '12', '13']
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

    crit_beddays_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    crit_beddays = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_beddays_emadm_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D']
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

    crit_beddays_eladm_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['11', '12', '13']
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

    opapp_binary = patients.outpatient_appointment_date(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    opapp = patients.outpatient_appointment_date(
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    opatt_binary = patients.outpatient_appointment_date(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"],
        attended = True
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
    gp_binary = patients.with_gp_consultations(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

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
    eol_med_binary = patients.with_these_medications(
        midazolam_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    eol_med = patients.with_these_medications(
        midazolam_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Respite care
    respite_binary = patients.with_these_clinical_events(
        respite_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    respite = patients.with_these_clinical_events(
        respite_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    #**loop_over_codes(respite_codes),

    ## Hospice care
    hospice_binary = patients.with_these_clinical_events(
        hospice_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    hospice = patients.with_these_clinical_events(
        hospice_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Ambulance incidents
    ambulance_binary = patients.with_these_clinical_events(
        ambulance_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    ambulance = patients.with_these_clinical_events(
        ambulance_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Community nursing
    nursing_binary = patients.with_these_clinical_events(
        community_nursing_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    nursing = patients.with_these_clinical_events(
        community_nursing_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## MDT
    mdt_binary = patients.with_these_clinical_events(
        mdt_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    mdt = patients.with_these_clinical_events(
        mdt_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Cancer MDT
    cancer_mdt_binary = patients.with_these_clinical_events(
        cancer_mdt_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    cancer_mdt = patients.with_these_clinical_events(
        cancer_mdt_codes,
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
        id="aevis_by_practice_binary",
        numerator="aevis_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="adm_by_practice_binary",
        numerator="adm_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="emadm_by_practice_binary",
        numerator="emadm_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="eladm_by_practice_binary",
        numerator="eladm_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="beddays_by_practice_binary",
        numerator="beddays_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="beddays_emadm_by_practice_binary",
        numerator="beddays_emadm_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="beddays_eladm_by_practice_binary",
        numerator="beddays_eladm_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="crit_beddays_by_practice_binary",
        numerator="crit_beddays_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="crit_beddays_emadm_by_practice_binary",
        numerator="crit_beddays_emadm_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="crit_beddays_eladm_by_practice_binary",
        numerator="crit_beddays_eladm_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="opapp_by_practice_binary",
        numerator="opapp_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="opatt_by_practice_binary",
        numerator="opatt_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="gp_by_practice_binary",
        numerator="gp_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="eol_med_by_practice_binary",
        numerator="eol_med_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="respite_by_practice_binary",
        numerator="respite_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="hospice_by_practice_binary",
        numerator="hospice_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="ambulance_by_practice_binary",
        numerator="ambulance_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="nursing_by_practice_binary",
        numerator="nursing_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="mdt_by_practice_binary",
        numerator="mdt_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="cancer_mdt_by_practice_binary",
        numerator="cancer_mdt_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="aevis_by_practice_rate",
        numerator="aevis",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="adm_by_practice_rate",
        numerator="adm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="emadm_by_practice_rate",
        numerator="emadm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="eladm_by_practice_rate",
        numerator="eladm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="beddays_by_practice_rate",
        numerator="beddays",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="beddays_emadm_by_practice_rate",
        numerator="beddays_emadm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="beddays_eladm_by_practice_rate",
        numerator="beddays_eladm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="crit_beddays_by_practice_rate",
        numerator="crit_beddays",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="crit_beddays_emadm_by_practice_rate",
        numerator="crit_beddays_emadm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="crit_beddays_eladm_by_practice_rate",
        numerator="crit_beddays_eladm",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="opapp_by_practice_rate",
        numerator="opapp",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="opatt_by_practice_rate",
        numerator="opatt",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="gp_by_practice_rate",
        numerator="gp",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="eol_med_by_practice_rate",
        numerator="eol_med",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="respite_by_practice_rate",
        numerator="respite",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="hospice_by_practice_rate",
        numerator="hospice",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="ambulance_by_practice_rate",
        numerator="ambulance",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="nursing_by_practice_rate",
        numerator="nursing",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="mdt_by_practice_rate",
        numerator="mdt",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="cancer_mdt_by_practice_rate",
        numerator="cancer_mdt",
        denominator="population",
        group_by="practice_id",
    )

]
