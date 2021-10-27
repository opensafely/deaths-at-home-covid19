from cohortextractor import StudyDefinition, patients, codelist, codelist_from_csv  # NOQA


study = StudyDefinition(
    default_expectations={
        "date": {"earliest": "1900-01-01", "latest": "today"},
        "rate": "uniform",
        "incidence": 0.5,
    },

    population=patients.registered_with_one_practice_between(
        "2018-03-01", "2020-02-28"
    ),

    death_date=patients.died_from_any_cause(
        between=["2019-03-01", "2021-02-28"],
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": "2019-03-01"},
            "rate": "exponential_increase"
        },
    ),

    sex=patients.sex(
        return_expectations={
            "rate": "universal",
            "category": {"ratios": {"M": 0.49, "F": 0.51}},
        },
    ),

    age=patients.age_as_of(
        "2019-09-01",
        return_expectations={
            "rate": "universal",
            "int": {"distribution": "population_ages"},
        },
    ),

    admission=patients.admitted_to_hospital(
        returning="binary_flag",
        on_or_before="death_date",
        find_first_match_in_period=True,
        date_format="YYYY-MM-DD",
        return_expectations={"date": {"earliest": "2018-03-01"}},
    ),
)

# How to refer to created variables when creating new?
