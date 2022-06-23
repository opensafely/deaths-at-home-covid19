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
    population = patients.all(),
        # """
        # has_died 
        # AND 
        # registered 
        # AND 
        # (sex = "F" OR sex = "M")
        # """
    # ),  

    ## CREATE VARIABLES ##

    has_died = patients.died_from_any_cause(
        between = [EARLIEST, LATEST],
        return_expectations = {"incidence": 0.50}
    ),

    ## ONS date of death
    # Want it present for 100% of individuals
    dod_ons = patients.died_from_any_cause(
        between = [EARLIEST, LATEST],
        returning = "date_of_death",
        date_format = "YYYY-MM-DD",
        return_expectations = {
            "date": {"earliest": EARLIEST},
            "rate": "uniform",
            "incidence": 0.50
        }
    ),

    registered = patients.registered_as_of(
        "dod_ons",
        return_expectations = {"incidence": 0.98}
    ),

    ## Demographics ##

    ## Sex - 49% male
    sex = patients.sex(
        return_expectations = {
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.49, "U": 0.02}}
        }
    )
)
