import json
import pandas as pd

df = pd.read_csv("output/dataset.csv")

# In period 1, how many patients were captured by dm+d codelists?
num_patients_dmd_p1 = len(df[df["dmd_p1"] > 0])

# In period 1, how many patients were not captured by dm+d codelists, but would have
# been if the codelist contained medications that have only have multilex codes?
num_patients_multilex_only_p1 = len(df[(df["dmd_p1"] == 0) & (df["multilex_p1"] > 0)])

# As above, for peroid 2.
num_patients_dmd_p2 = len(df[df["dmd_p2"] > 0])
num_patients_multilex_only_p2 = len(df[(df["dmd_p2"] == 0) & (df["multilex_p2"] > 0)])

counts = {
    "num_patients_dmd_p1": num_patients_dmd_p1,
    "num_patients_multilex_only_p1": num_patients_multilex_only_p1,
    "num_patients_dmd_p2": num_patients_dmd_p2,
    "num_patients_multilex_only_p2": num_patients_multilex_only_p2,
}

with open("output/medication-counts.json", "w") as f:
    json.dump(counts, f, indent=2)
