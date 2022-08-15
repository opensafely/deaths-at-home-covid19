# Functions from cohort extractor

from cohortextractor import (
    StudyDefinition,
    patients)

## CODELISTS ##

# Import codelists from the codelist folder

from codelists import *

## KEY VARIABLES ##

EARLIEST = "2019-03-01"
LATEST = "2021-02-28"

## STUDY DEFINITION ##

study = StudyDefinition(

    ## Default variable expectations 
    # e.g. dates between 1/1/1970 to 28/2/2021 with uniform frequency and 50% present
    # e.g. categorical and numeric present 50% 
    # e.g. binary 50% positive
    default_expectations = {
        "date": {"earliest": "1970-01-01", "latest": LATEST},
        "rate": "uniform",
        "incidence": 0.5
    },

    ## Study population
    # Everyone who died between March 2019 and February 2021
    # Registered with TPP on date of death
    population = patients.satisfying(
        """
        has_died 
        AND 
        registered 
        AND 
        (sex = "F" OR sex = "M")
        """,
        has_died = patients.died_from_any_cause(
            between = [EARLIEST, LATEST],
            return_expectations = {"incidence": 1.0}
        ),
        registered = patients.registered_as_of(
            "dod_ons",
            return_expectations = {"incidence": 0.98}
        )
    ),  

    ## CREATE VARIABLES ##

    ## Key cohort variables ##

    ## ONS date of death
    # Want it present for 100% of individuals
    dod_ons = patients.died_from_any_cause(
        between = [EARLIEST, LATEST],
        returning = "date_of_death",
        date_format = "YYYY-MM-DD",
        return_expectations = {
            "date": {"earliest": EARLIEST},
            "rate": "uniform",
            "incidence": 1.0
        }
    ),

    ## ONS place of death 
    # Distribution from English death locations
    pod_ons=patients.died_from_any_cause(
       returning="place_of_death",
       return_expectations={
           "rate": "universal",
           "category": {"ratios": {"Care home": 0.236, "Elsewhere": 0.022, "Home": 0.274, "Hospice": 0.044, "Hospital": 0.42, "Other communal establishment": 0.004}}
       }
    ),

    ## ONS underlying cause of death 
    # Will want to create a categorised version of this later e.g. sudden deaths
    cod_ons = patients.died_from_any_cause(
        returning = "underlying_cause_of_death",
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"U071":0.03, "J4":0.1, "F01":0.06, "J1":0.11 , "I2":0.43 , "C3":0.13, "A1":0.14}}
        }
    ),

    ## Demographics ##

    ## Sex - 49% male
    sex = patients.sex(
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}}
        }
    ),

    ## Age based on population distribution
    age = patients.age_as_of(
        "dod_ons",
        return_expectations = {
            "int" : {"distribution": "normal", "mean": 60, "stddev": 15}, 
            "incidence" : 1.0
        }
    ),

    ## Ethnicity -  6 categories coded in primary care
    ethnicity_gp = patients.with_these_clinical_events(
        ethnicity_codes_6,
        returning = "category",
        find_last_match_in_period = True,
        return_expectations = {
            "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
            "incidence": 0.75
        }
    ),

    ## Ethnicity -  6 categories coded in SUS
    ethnicity_sus = patients.with_ethnicity_from_sus(
        returning = "group_6",
        use_most_frequent_code = True,
        return_expectations = {
            "category": {"ratios": {"1": 0.2, "2": 0.2, "3": 0.2, "4": 0.2, "5": 0.2}},
            "incidence": 0.75
        }
    ),

    ethnicity = patients.categorised_as(
        {"0": "DEFAULT",
        "1": "ethnicity_gp = '1' OR (NOT ethnicity_gp AND ethnicity_sus = '1')",
        "2": "ethnicity_gp = '2' OR (NOT ethnicity_gp AND ethnicity_sus = '2')",
        "3": "ethnicity_gp = '3' OR (NOT ethnicity_gp AND ethnicity_sus = '3')",
        "4": "ethnicity_gp = '4' OR (NOT ethnicity_gp AND ethnicity_sus = '4')",
        "5": "ethnicity_gp = '5' OR (NOT ethnicity_gp AND ethnicity_sus = '5')"
        },
        return_expectations = {
            "category": {"ratios": {"0": 0.05, "1": 0.19, "2": 0.19, "3": 0.19, "4": 0.19, "5": 0.19}},
            "incidence": 1.0
        }
    ),

    ## Geography ##

    ## MSOA of patient address 
    msoa = patients.address_as_of(
        "dod_ons",
        returning = "msoa",
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"E02000001": 0.5, "E02000002": 0.5}}
        }
    ),

    ## Rural/urban class of patient address
    ## MSOA of patient address 
    rural_class = patients.address_as_of(
        "dod_ons",
        returning = "rural_urban_classification",
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {1: 0.125, 2: 0.125, 3: 0.125, 4: 0.125, 5: 0.125, 6: 0.125, 7: 0.125, 8: 0.125}}
        }
    ),

    ## Index of multiple deprivation based on patient address
    imd_quintile = patients.categorised_as(
        {
            "0": "DEFAULT",
            "1": """index_of_multiple_deprivation >= 1 AND index_of_multiple_deprivation < 32844*1/5""",
            "2": """index_of_multiple_deprivation >= 32844*1/5 AND index_of_multiple_deprivation < 32844*2/5""",
            "3": """index_of_multiple_deprivation >= 32844*2/5 AND index_of_multiple_deprivation < 32844*3/5""",
            "4": """index_of_multiple_deprivation >= 32844*3/5 AND index_of_multiple_deprivation < 32844*4/5""",
            "5": """index_of_multiple_deprivation >= 32844*4/5 AND index_of_multiple_deprivation < 32844"""
        },
        index_of_multiple_deprivation = patients.address_as_of(
            "dod_ons",
            returning = "index_of_multiple_deprivation",
            round_to_nearest = 100
        ),
        return_expectations = {
            "rate": "universal",
            "category": {
                "ratios": {
                    "0": 0.05,
                    "1": 0.19, 
                    "2": 0.19,
                    "3": 0.19,
                    "4": 0.19,
                    "5": 0.19
                }
            }
        }
    ),

    ## Care home type - TPP assigned 
    carehome = patients.care_home_status_as_of(
        "dod_ons",
        categorised_as = {
            "PC":
            """
            IsPotentialCareHome
            AND LocationDoesNotRequireNursing = 'Y'
            AND LocationRequiresNursing = 'N'
            """,
            "PN":
            """
            IsPotentialCareHome
            AND LocationDoesNotRequireNursing = 'N'
            AND LocationRequiresNursing = 'Y'
            """,
            "PS": "IsPotentialCareHome",
            "PR": "NOT IsPotentialCareHome",
            "": "DEFAULT",
        },
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"PC": 0.05, "PN": 0.05, "PS": 0.05, "PR": 0.84, "": 0.01}}
            }
    ),

    ## MSOA of gp
    # As an alternative to MSOA from patient address
    msoa_gp = patients.registered_practice_as_of(
        "dod_ons",
        returning = "msoa",
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"E02000001": 0.5, "E02000002": 0.5}}
        }
    ),

    ## Region of gp
    # As an alternative to region from patient address
    region_gp = patients.registered_practice_as_of(
        "dod_ons",
        returning = "nuts1_region_name",
        return_expectations={
        "rate": "universal",
        "category": {
            "ratios": {
                "North East": 0.1,
                "North West": 0.1,
                "Yorkshire and the Humber": 0.1,
                "East Midlands": 0.1,
                "West Midlands": 0.1,
                "East of England": 0.1,
                "London": 0.2,
                "South East": 0.2,}}
                }
    ),

    ## Health ##

    ## COVID-19 positive
    covid_pos = patients.with_test_result_in_sgss(
        pathogen = "SARS-CoV-2",
        test_result = "positive",
        returning = "binary_flag"
    ),

    ## Long term conditions
    # Check over the five years prior to death

    ltc = patients.with_these_clinical_events(
        ltc_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    physical_ltc = patients.with_these_clinical_events(
        physical_ltc_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    mental_ltc = patients.with_these_clinical_events(
        mental_ltc_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Atrial fibrillation
    ltc_afib = patients.with_these_clinical_events(
        afib_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),
    # Coronary heart disease
    ltc_chd = patients.with_these_clinical_events(
        chd_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Heart failure 
    ltc_hf = patients.with_these_clinical_events(
        hf_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Hypertension
    ltc_hyp = patients.with_these_clinical_events(
        hyp_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Peripheral arterial disease
    ltc_pad = patients.with_these_clinical_events(
        pad_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Stroke and transient ischaemic attack
    ltc_strk = patients.with_these_clinical_events(
        strk_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Asthma
    ltc_ast = patients.with_these_clinical_events(
        ast_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # COPD
    ltc_copd = patients.with_these_clinical_events(
        copd_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Cancer
    ltc_haemcan = patients.with_these_clinical_events(
        haemcan_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    ltc_can = patients.with_these_clinical_events(
        can_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Chronic kidney disease
    ltc_ckd1 = patients.with_these_clinical_events(
        ckd_codes1,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    ltc_ckd2 = patients.with_these_clinical_events(
        ckd_codes2,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Diabetes mellitus
    ltc_dm = patients.with_these_clinical_events(
        dm_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Palliative care
    ltc_palcare1 = patients.with_these_clinical_events(
        palcare_codes1,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    ltc_palcare2 = patients.with_these_clinical_events(
        palcare_codes2,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Epilepsy
    ltc_epil = patients.with_these_clinical_events(
        epil_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Osteoporosis
    ltc_osteo = patients.with_these_clinical_events(
        osteo_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Rheumatoid arthiritis
    ltc_rarth = patients.with_these_clinical_events(
        rarth_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Non-diabetic hyperglycaemia? 
    ltc_ndh = patients.with_these_clinical_events(
        ndh_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Dementia
    ltc_dem = patients.with_these_clinical_events(
        dem_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Depression
    ltc_depr = patients.with_these_clinical_events(
        depr_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Schizophrenia, Bipolar effective disorder, Psychoses
    ltc_mh = patients.with_these_clinical_events(
        mh_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    # Learning disability?
    ltc_ld = patients.with_these_clinical_events(
        ld_codes,
        between = ["dod_ons - 1825 days", "dod_ons"],
        returning = "binary_flag"
    ),

    ## SERVICE USE ##

    ## Hospital activity in 1 month, 3 months and 1 year prior to death
    aevis_1m = patients.attended_emergency_care(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    aevis_3m = patients.attended_emergency_care(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    aevis_1y = patients.attended_emergency_care(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    adm_1m = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    adm_3m = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    adm_1y = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    emadm_1m = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    emadm_3m = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    emadm_1y = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    eladm_1m = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    eladm_3m = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    eladm_1y = patients.admitted_to_hospital(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    beddays_1m = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    beddays_3m = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    beddays_1y = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    embeddays_1m = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    embeddays_3m = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    embeddays_1y = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    elbeddays_1m = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    elbeddays_3m = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    elbeddays_1y = patients.admitted_to_hospital(
        returning = "total_bed_days_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_beddays_1m = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_beddays_3m = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_beddays_1y = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_embeddays_1m = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_embeddays_3m = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_embeddays_1y = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        with_admission_method = ['21', '2A', '22', '23', '24', '25', '2D'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_elbeddays_1m = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_elbeddays_3m = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    crit_elbeddays_1y = patients.admitted_to_hospital(
        returning = "total_critical_care_days_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        with_admission_method = ['11', '12', '13'],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    opapp_1m = patients.outpatient_appointment_date(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    opapp_3m = patients.outpatient_appointment_date(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    opapp_1y = patients.outpatient_appointment_date(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    opatt_1m = patients.outpatient_appointment_date(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        attended = True,
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    opatt_3m = patients.outpatient_appointment_date(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        attended = True,
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    opatt_1y = patients.outpatient_appointment_date(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        attended = True,
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Flag for people with complete gp history
    # Some measures of activity drawn from GP record will be affected if people change practices/switch to TPP
    gp_hist_1m = patients.with_complete_gp_consultation_history_between(
       "dod_ons - 30 days", "dod_ons", return_expectations={"incidence": 0.9}
    ),

    gp_hist_3m = patients.with_complete_gp_consultation_history_between(
       "dod_ons - 90 days", "dod_ons", return_expectations={"incidence": 0.9}
    ),

    gp_hist_1y = patients.with_complete_gp_consultation_history_between(
       "dod_ons - 365 days", "dod_ons", return_expectations={"incidence": 0.9}
    ),

    ## GP consultations
    # Consultation can include things like phone number update  
    gp_1m = patients.with_gp_consultations(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    gp_3m = patients.with_gp_consultations(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    gp_1y = patients.with_gp_consultations(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## EOL medication
    eol_med_1m = patients.with_these_medications(
        eol_med_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    eol_med_3m = patients.with_these_medications(
        eol_med_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    eol_med_1y = patients.with_these_medications(
       eol_med_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    #Codelist with additional meds beyond priority list
    eol_med_full_1m = patients.with_these_medications(
        eol_med_full_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    eol_med_full_3m = patients.with_these_medications(
        eol_med_full_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    eol_med_full_1y = patients.with_these_medications(
       eol_med_full_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    midazolam_1m = patients.with_these_medications(
        midazolam_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    midazolam_3m = patients.with_these_medications(
        midazolam_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    midazolam_1y = patients.with_these_medications(
       midazolam_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    glycopyrronium_1m = patients.with_these_medications(
        glycopyrronium_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    glycopyrronium_3m = patients.with_these_medications(
        glycopyrronium_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    glycopyrronium_1y = patients.with_these_medications(
       glycopyrronium_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    haloperidol_1m = patients.with_these_medications(
        haloperidol_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    haloperidol_3m = patients.with_these_medications(
        haloperidol_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    haloperidol_1y = patients.with_these_medications(
       haloperidol_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    hyoscine_butyl_1m = patients.with_these_medications(
        hyoscine_butylbromide_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    hyoscine_butyl_3m = patients.with_these_medications(
        hyoscine_butylbromide_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    hyoscine_butyl_1y = patients.with_these_medications(
       hyoscine_butylbromide_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    levomepromazine_1m = patients.with_these_medications(
        levomepromazine_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    levomepromazine_3m = patients.with_these_medications(
        levomepromazine_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    levomepromazine_1y = patients.with_these_medications(
       levomepromazine_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    morphine_1m = patients.with_these_medications(
        morphine_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    morphine_3m = patients.with_these_medications(
        morphine_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    morphine_1y = patients.with_these_medications(
       morphine_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    oxycodone_1m = patients.with_these_medications(
        oxycodone_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    oxycodone_3m = patients.with_these_medications(
        oxycodone_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    oxycodone_1y = patients.with_these_medications(
       oxycodone_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    hyoscine_hydro_1m = patients.with_these_medications(
        hyoscine_hydrobromide_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    hyoscine_hydro_3m = patients.with_these_medications(
        hyoscine_hydrobromide_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    hyoscine_hydro_1y = patients.with_these_medications(
       hyoscine_hydrobromide_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    alfentanil_1m = patients.with_these_medications(
        alfentanil_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    alfentanil_3m = patients.with_these_medications(
        alfentanil_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    alfentanil_1y = patients.with_these_medications(
       alfentanil_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    diamorphine_1m = patients.with_these_medications(
        diamorphine_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    diamorphine_3m = patients.with_these_medications(
        diamorphine_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    diamorphine_1y = patients.with_these_medications(
       diamorphine_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Palliative care
    palliative_1m = patients.with_these_clinical_events(
        palcare_codes1,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    palliative_3m = patients.with_these_clinical_events(
        palcare_codes1,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    palliative_1y = patients.with_these_clinical_events(
        palcare_codes1,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Respite care
    respite_1m = patients.with_these_clinical_events(
        respite_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    respite_3m = patients.with_these_clinical_events(
        respite_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    respite_1y = patients.with_these_clinical_events(
        respite_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Hospice care
    hospice_1m = patients.with_these_clinical_events(
        hospice_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    hospice_3m = patients.with_these_clinical_events(
        hospice_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    hospice_1y = patients.with_these_clinical_events(
        hospice_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Ambulance incidents
    ambulance_1m = patients.with_these_clinical_events(
        ambulance_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    ambulance_3m = patients.with_these_clinical_events(
        ambulance_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    ambulance_1y = patients.with_these_clinical_events(
        ambulance_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Community nursing
    nursing_1m = patients.with_these_clinical_events(
        community_nursing_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    nursing_3m = patients.with_these_clinical_events(
        community_nursing_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    nursing_1y = patients.with_these_clinical_events(
        community_nursing_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## MDT
    mdt_1m = patients.with_these_clinical_events(
        mdt_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    mdt_3m = patients.with_these_clinical_events(
        mdt_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    mdt_1y = patients.with_these_clinical_events(
        mdt_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Cancer MDT
    cancer_mdt_1m = patients.with_these_clinical_events(
        cancer_mdt_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    cancer_mdt_3m = patients.with_these_clinical_events(
        cancer_mdt_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    cancer_mdt_1y = patients.with_these_clinical_events(
        cancer_mdt_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## MDT care
    all_mdt_1m = patients.with_these_clinical_events(
        all_mdt_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    all_mdt_3m = patients.with_these_clinical_events(
        all_mdt_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    all_mdt_1y = patients.with_these_clinical_events(
        all_mdt_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),

    ## Community care
    community_1m = patients.with_these_clinical_events(
        community_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    community_3m = patients.with_these_clinical_events(
        community_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 90 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    ),
    
    community_1y = patients.with_these_clinical_events(
        community_codes,
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 365 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    )              

)

# opensafely run run_all --force-run-dependencies
# opensafely upgrade
# opensafely pull
