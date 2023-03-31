import json
import pandas as pd


def main(df):
    return round_to_nearest_10(main_unrounded(df))


def main_unrounded(df):
    assert (df["died_in_p1"] | df["died_in_p2"]).all()

    return {
        "p1_died": counts(df[df["died_in_p1"]]),
        "p1_died_at_home": counts(df[df["died_in_p1"] & df["died_at_home"]]),
        "p2_died": counts(df[df["died_in_p2"]]),
        "p2_died_at_home": counts(df[df["died_in_p2"] & df["died_at_home"]]),
    }


def counts(df):
    return {str(m): counts_m(df, m) for m in [1, 3, 12]}


def counts_m(df, m):
    results = {}

    has_dmd = df[f"dmd_{m}"] > 0
    dmd_counts = df[has_dmd][f"dmd_{m}"]
    # How many patients have had a prescription for a mediciation in the dm+d
    # codelist in the m months before death?
    results[f"dmd_patients"] = len(dmd_counts)
    # How many prescription for a mediciation in the dm+d codelist were there in the
    # m months before death?
    results[f"dmd_prescriptions"] = int(dmd_counts.sum())

    has_multilex = df[f"multilex_{m}"] > 0
    multilex_counts = df[has_multilex][f"multilex_{m}"]
    # How many patients have had a prescription for a mediciation in the multilex
    # codelist in the m months before death?
    results[f"multilex_patients"] = len(multilex_counts)
    # How many prescription for a mediciation in the dm+d codelist were there in the
    # m months before death?
    results[f"multilex_prescriptions"] = int(multilex_counts.sum())

    has_only_multilex = (df[f"multilex_{m}"] > 0) & (df[f"dmd_{m}"] == 0)
    multilex_only_counts = df[has_only_multilex][f"multilex_{m}"]
    # How many patients have had a prescription for a mediciation in the multilex
    # codelist, but not a prescription for a medication in the dm+d codelist, in the
    # m months before death?
    results[f"multilex_only_patients"] = len(multilex_only_counts)
    # How many prescriptions were there for patients have had a prescription for a
    # mediciation in the multilex codelist, but not a prescription for a medication in
    # the dm+d codelist, in the m months before death?
    results[f"multilex_only_prescriptions"] = int(multilex_only_counts.sum())

    return results


def round_to_nearest_10(value):
    if isinstance(value, dict):
        return {k: round_to_nearest_10(v) for k, v in value.items()}
    elif isinstance(value, int):
        return round(value, -1)
    else:
        assert False, value


if __name__ == "__main__":
    with open("output/medication-counts.json", "w") as f:
        json.dump(main(pd.read_feather("output/dataset.arrow")), f, indent=2)
