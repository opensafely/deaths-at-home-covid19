# Creates the population needed for the analysis to feed the OS report

# Functions from cohort extractor

from cohortextractor import (
    StudyDefinition,
    patients)

## CODELISTS ##

# Import codelists from the codelist folder

from codelists import *

## KEY VARIABLES ##

EARLIEST = "2019-06-01"
LATEST = "2023-06-30"

## STUDY DEFINITION ##

study = StudyDefinition(

    ## Default variable expectations unless specified
    # e.g. dates between 1/1/1970 to 30/06/2023 with uniform frequency and 50% present
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
            "category": {"ratios": {"U071":0.16, "J4":0.06, "F01":0.10, "J1":0.02 , "I2":0.22 , "C3":0.24, "A1":0.20}}
        }
    ),
    
    ## Demographics ##

    ## Sex - 50% male
    sex = patients.sex(
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"M": 0.50, "F": 0.50}}
        }
    ),

    ## SERVICE USE ##

    ## GP consultations
    # Consultation can include things like phone number update  
    gp_1m = patients.with_gp_consultations(
        returning = "number_of_matches_in_period",
        between = ["dod_ons - 30 days", "dod_ons"],
        return_expectations = {
            "int": {"distribution": "normal", "mean": 5, "stddev": 1}, 
            "incidence": 0.8
            }
    )

)
