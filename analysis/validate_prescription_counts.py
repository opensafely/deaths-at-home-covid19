import pandas as pd
from pathlib import Path

output = Path("output")

df = pd.read_csv(output / "dataset.csv")

columns = [
    "morphine_subcutaneous_dmd_count",
    "morphine_subcutaneous_dmd_updated_count",
    "morphine_subcutaneous_multilex_count",
]

df[columns].sum().reset_index().to_csv(
    output / "validation_totals.csv", index=False, header=False
)
