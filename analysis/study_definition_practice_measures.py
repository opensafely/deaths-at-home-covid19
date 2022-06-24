# Checking data quality of service use measures - distribution by practice and month

from cohortextractor import (
    StudyDefinition, 
    Measure,
#    codelist, 
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
#                between = ["index_date", "last_day_of_month(index_date)"],
#                returning = "number_of_matches_in_period",
#                return_expectations = {
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

    embeddays_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D']
    ),

    embeddays = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    elbeddays_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['11', '12', '13']
    ),

    elbeddays = patients.admitted_to_hospital(
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

    crit_embeddays_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D']
    ),

    crit_embeddays = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_elbeddays_binary = patients.admitted_to_hospital(
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"],
        with_admission_method = ['11', '12', '13']
    ),

    crit_elbeddays = patients.admitted_to_hospital(
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
        eol_med_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    eol_med = patients.with_these_medications(
        eol_med_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    #Codelist with additional meds beyond priority list
    eol_med_full_binary = patients.with_these_medications(
        eol_med_full_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    eol_med_full = patients.with_these_medications(
        eol_med_full_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    midazolam_binary = patients.with_these_medications(
        midazolam_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),
    
    midazolam = patients.with_these_medications(
        midazolam_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    glycopyrronium_binary = patients.with_these_medications(
        glycopyrronium_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),
    
    glycopyrronium = patients.with_these_medications(
        glycopyrronium_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    haloperidol_binary = patients.with_these_medications(
        haloperidol_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),
    
    haloperidol = patients.with_these_medications(
        haloperidol_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    hyoscine_butyl_binary = patients.with_these_medications(
        hyoscine_butylbromide_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),
    
    hyoscine_butyl = patients.with_these_medications(
        hyoscine_butylbromide_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    levomepromazine_binary = patients.with_these_medications(
        levomepromazine_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),
    
    levomepromazine = patients.with_these_medications(
        levomepromazine_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    morphine_binary = patients.with_these_medications(
        morphine_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),
    
    morphine = patients.with_these_medications(
        morphine_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    oxycodone_binary = patients.with_these_medications(
        oxycodone_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),
    
    oxycodone = patients.with_these_medications(
        oxycodone_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    hyoscine_hydro_binary = patients.with_these_medications(
        hyoscine_hydrobromide_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),
    
    hyoscine_hydro = patients.with_these_medications(
        hyoscine_hydrobromide_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    alfentanil_binary = patients.with_these_medications(
        alfentanil_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),
    
    alfentanil = patients.with_these_medications(
        alfentanil_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    diamorphine_binary = patients.with_these_medications(
        diamorphine_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),
    
    diamorphine = patients.with_these_medications(
        diamorphine_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Palliative care
    palliative_binary = patients.with_these_clinical_events(
        palcare_codes1,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),


    palliative = patients.with_these_clinical_events(
        palcare_codes1,
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
    ),

    ## All MDT
    all_mdt_binary = patients.with_these_clinical_events(
        all_mdt_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    all_mdt = patients.with_these_clinical_events(
        all_mdt_codes,
        returning = "number_of_matches_in_period",
        between = ["index_date", "last_day_of_month(index_date)"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Community
    community_binary = patients.with_these_clinical_events(
        community_codes,
        returning = "binary_flag",
        between = ["index_date", "last_day_of_month(index_date)"]
    ),

    community = patients.with_these_clinical_events(
        community_codes,
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
        id="embeddays_by_practice_binary",
        numerator="embeddays_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="elbeddays_by_practice_binary",
        numerator="elbeddays_binary",
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
        id="crit_embeddays_by_practice_binary",
        numerator="crit_embeddays_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="crit_elbeddays_by_practice_binary",
        numerator="crit_elbeddays_binary",
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
        id="eol_med_full_by_practice_binary",
        numerator="eol_med_full_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="midazolam_by_practice_binary",
        numerator="midazolam_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="glycopyrronium_by_practice_binary",
        numerator="glycopyrronium_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="haloperidol_by_practice_binary",
        numerator="haloperidol_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="hyoscine_butly_by_practice_binary",
        numerator="hyoscine_butyl_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="levomepromazine_by_practice_binary",
        numerator="levomepromazine_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="morphine_by_practice_binary",
        numerator="morphine_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="oxycodone_by_practice_binary",
        numerator="oxycodone_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="hyoscine_hydro_by_practice_binary",
        numerator="hyoscine_hydro_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="alfentanil_by_practice_binary",
        numerator="alfentanil_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="diamorphine_by_practice_binary",
        numerator="diamorphine_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="palliative_by_practice_binary",
        numerator="palliative_binary",
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
        id="all_mdt_by_practice_binary",
        numerator="all_mdt_binary",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="community_by_practice_binary",
        numerator="community_binary",
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
        id="embeddays_by_practice_rate",
        numerator="embeddays",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="elbeddays_by_practice_rate",
        numerator="elbeddays",
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
        id="crit_embeddays_by_practice_rate",
        numerator="crit_embeddays",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="crit_elbeddays_by_practice_rate",
        numerator="crit_elbeddays",
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
        id="eol_med_full_by_practice_rate",
        numerator="eol_med_full",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="midazolam_by_practice_rate",
        numerator="midazolam",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="glycopyrronium_by_practice_rate",
        numerator="glycopyrronium",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="haloperidol_by_practice_rate",
        numerator="haloperidol",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="hyoscine_butly_by_practice_rate",
        numerator="hyoscine_butyl",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="levomepromazine_by_practice_rate",
        numerator="levomepromazine",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="morphine_by_practice_rate",
        numerator="morphine",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="oxycodone_by_practice_rate",
        numerator="oxycodone",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="hyoscine_hydro_by_practice_rate",
        numerator="hyoscine_hydro",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="alfentanil_by_practice_rate",
        numerator="alfentanil",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="diamorphine_by_practice_rate",
        numerator="diamorphine",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="palliative_by_practice_rate",
        numerator="palliative",
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
    ),

    Measure(
        id="all_mdt_by_practice_rate",
        numerator="all_mdt",
        denominator="population",
        group_by="practice_id",
    ),

    Measure(
        id="community_by_practice_rate",
        numerator="community",
        denominator="population",
        group_by="practice_id",
    )

]
