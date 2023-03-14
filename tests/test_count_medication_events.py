import pandas as pd
from analysis.count_medication_events import counts_m


def test_counts_m():
    rows = [
        {"dmd_1": 3, "multilex_1": 3},
        {"dmd_1": 2, "multilex_1": 0},
        {"dmd_1": 0, "multilex_1": 1},
    ]

    df = pd.DataFrame(rows)

    assert counts_m(df, 1) == {
        "dmd_patients": 2,
        "dmd_prescriptions": 5,
        "multilex_patients": 2,
        "multilex_prescriptions": 4,
        "multilex_only_patients": 1,
        "multilex_only_prescriptions": 1,
    }
