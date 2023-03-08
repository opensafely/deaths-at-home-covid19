import json
import pandas as pd


def cohort_counts():
    df = pd.read_csv("output/cohort.csv")

    # Remap all columns to booleans
    for col in [
        "died_in_p1",
        "died_in_p2",
        "dmd_1",
        "dmd_3",
        "dmd_12",
    ]:
        df[col] = df[col] == 1

    # Pull out the two populations into separate dataframes
    df1 = df[df["died_in_p1"]]
    df2 = df[df["died_in_p2"]]

    # For each period, count how many patients had a medication in the dm+d codelist in
    # the 1 / 3 / 12 months before death.
    return {
        "p1": {
            "dmd_1": len(df1[df1["dmd_1"]]),
            "dmd_3": len(df1[df1["dmd_3"]]),
            "dmd_12": len(df1[df1["dmd_12"]]),
        },
        "p2": {
            "dmd_1": len(df2[df2["dmd_1"]]),
            "dmd_3": len(df2[df2["dmd_3"]]),
            "dmd_12": len(df2[df2["dmd_12"]]),
        },
    }


def dataset_counts():
    df = pd.read_csv("output/dataset.csv")

    # Remap all columns to booleans
    for col in [
        "died_in_p1",
        "died_in_p2",
        "dmd_1",
        "multilex_1",
        "dmd_3",
        "multilex_3",
        "dmd_12",
        "multilex_12",
    ]:
        df[col] = df[col] == "T"

    # Double check that all records are for a patient who has died
    assert (df["died_in_p1"] | df["died_in_p2"]).all()

    # Pull out the two populations into separate dataframes
    df1 = df[df["died_in_p1"]]
    df2 = df[df["died_in_p2"]]

    # For each period, count how many patients:
    #  * had a medication in the dm+d codelist
    #  * had a medication in the multilex codelist but not in the dm+d codelist
    # in the 1 / 3 / 12 months before death.
    return {
        "p1": {
            "dmd_1": len(df1[df1["dmd_1"]]),
            "multilex_only_1": len(df1[df1["multilex_1"] & ~df1["dmd_1"]]),
            "dmd_3": len(df1[df1["dmd_3"]]),
            "multilex_only_3": len(df1[df1["multilex_3"] & ~df1["dmd_3"]]),
            "dmd_12": len(df1[df1["dmd_12"]]),
            "multilex_only_12": len(df1[df1["multilex_12"] & ~df1["dmd_12"]]),
        },
        "p2": {
            "dmd_1": len(df2[df2["dmd_1"]]),
            "multilex_only_1": len(df2[df2["multilex_1"] & ~df2["dmd_1"]]),
            "dmd_3": len(df2[df2["dmd_3"]]),
            "multilex_only_3": len(df2[df2["multilex_3"] & ~df2["dmd_3"]]),
            "dmd_12": len(df2[df2["dmd_12"]]),
            "multilex_only_12": len(df2[df2["multilex_12"] & ~df2["dmd_12"]]),
        },
    }


counts = {
    "cohort": cohort_counts(),
    "dataset": dataset_counts(),
}


with open("output/medication-counts.json", "w") as f:
    json.dump(counts, f, indent=2)
