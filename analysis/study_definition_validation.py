from cohortextractor import StudyDefinition, patients
from cohortextractor import codelist_from_csv, combine_codelists

midazolam_codes = codelist_from_csv(
    "codelists/opensafely-midazolam-end-of-life.csv", system="snomed", column="dmd_id"
)
glycopyrronium_codes = codelist_from_csv(
    "codelists/opensafely-glycopyrronium-subcutaneous-formulations.csv",
    system="snomed",
    column="dmd_id",
)
haloperidol_codes = codelist_from_csv(
    "codelists/opensafely-haloperidol-subcutaneous-dmd.csv",
    system="snomed",
    column="dmd_id",
)
hyoscine_butylbromide_codes = codelist_from_csv(
    "codelists/opensafely-hyoscine-butylbromide-subcutaneous-formulations.csv",
    system="snomed",
    column="dmd_id",
)
levomepromazine_codes = codelist_from_csv(
    "codelists/opensafely-levomepromazine-subcutaneous.csv",
    system="snomed",
    column="dmd_id",
)
morphine_codes = codelist_from_csv(
    "codelists/opensafely-morphine-subcutaneous-dmd.csv",
    system="snomed",
    column="dmd_id",
)
oxycodone_codes = codelist_from_csv(
    "codelists/opensafely-oxycodone-subcutaneous-dmd.csv",
    system="snomed",
    column="dmd_id",
)

eol_med_codes = combine_codelists(
    midazolam_codes,
    glycopyrronium_codes,
    haloperidol_codes,
    hyoscine_butylbromide_codes,
    levomepromazine_codes,
    morphine_codes,
    oxycodone_codes,
)


# We're interested in events in two periods.
p1_date_range = ("2019-06-01", "2020-02-29")
p2_date_range = ("2020-06-01", "2021-02-28")
full_date_range = (p1_date_range[0], p2_date_range[1])

study = StudyDefinition(
    ## Default variable expectations
    # e.g. dates between 1/1/1970 to 28/2/2021 with uniform frequency and 50% present
    # e.g. categorical and numeric present 50%
    # e.g. binary 50% positive
    default_expectations={
        "date": {"earliest": "1970-01-01", "latest": full_date_range[1]},
        "rate": "uniform",
        "incidence": 0.5,
    },
    population=patients.satisfying(
        """
        has_died 
        AND 
        registered 
        AND 
        (sex = "F" OR sex = "M")
        """,
        has_died=patients.died_from_any_cause(
            between=full_date_range, return_expectations={"incidence": 1.0}
        ),
        registered=patients.registered_as_of(
            "date_of_death", return_expectations={"incidence": 0.98}
        ),
        sex=patients.sex(
            return_expectations={
                "rate": "universal",
                "category": {"ratios": {"M": 0.49, "F": 0.51}},
            }
        ),
    ),
    date_of_death=patients.died_from_any_cause(
        between=full_date_range,
        returning="date_of_death",
        date_format="YYYY-MM-DD",
        return_expectations={
            "date": {"earliest": full_date_range[0]},
            "rate": "uniform",
            "incidence": 1.0,
        },
    ),
    died_in_p1=patients.died_from_any_cause(between=p1_date_range),
    died_in_p2=patients.died_from_any_cause(between=p2_date_range),
    dmd_1=patients.with_these_medications(
        eol_med_codes,
        returning="binary_flag",
        between=["date_of_death - 30 days", "date_of_death"],
        return_expectations={
            "int": {"distribution": "normal", "mean": 5, "stddev": 1},
            "incidence": 0.8,
        },
    ),
    dmd_3=patients.with_these_medications(
        eol_med_codes,
        returning="binary_flag",
        between=["date_of_death - 90 days", "date_of_death"],
        return_expectations={
            "int": {"distribution": "normal", "mean": 5, "stddev": 1},
            "incidence": 0.8,
        },
    ),
    dmd_12=patients.with_these_medications(
        eol_med_codes,
        returning="binary_flag",
        between=["date_of_death - 365 days", "date_of_death"],
        return_expectations={
            "int": {"distribution": "normal", "mean": 5, "stddev": 1},
            "incidence": 0.8,
        },
    ),
)
