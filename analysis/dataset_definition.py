import csv
from pathlib import Path
from databuilder.codes import codelist_from_csv
from databuilder.ehrql import Dataset, years
from databuilder.tables.beta.tpp import (
    medications,
    ons_deaths,
    patients,
    practice_registrations,
)

CODELIST_DIR = Path("validation-codelists")


morphine_subcutaneous_dmd = codelist_from_csv(
    CODELIST_DIR / "opensafely-morphine-subcutaneous-dmd-1185fc5b.csv", "dmd_id"
)
morphine_subcutaneous_dmd_updated = codelist_from_csv(
    CODELIST_DIR / "opensafely-morphine-subcutaneous-dmd-69f036dd.csv", "dmd_id"
)

with open(CODELIST_DIR / "opensafely-morphine-subcutaneous-multilex.csv", "r") as cf:
    reader = csv.DictReader(cf, ["MultilexDrug_ID"])
    morphine_subcutaneous_multilex = list(reader)

EARLIEST = "2019-03-01"
LATEST = "2021-02-28"

dataset = Dataset()

date_of_death = ons_deaths.date
has_died = date_of_death.is_on_or_between(EARLIEST, LATEST)
is_registered = (
    practice_registrations.take(practice_registrations.date_start <= date_of_death)
    .drop(practice_registrations.date_end <= date_of_death)
    .exists_for_patient()
)

dataset.set_population(has_died & is_registered & (patients.sex.is_in(["F", "M"])))

# Medication events in year before death
relevant_medications = medications.take(
    medications.date.is_on_or_between(date_of_death - years(1), date_of_death)
)

dataset.morphine_subcutaneous_dmd_count = relevant_medications.take(
    relevant_medications.dmd_code.is_in(morphine_subcutaneous_dmd)
).count_for_patient()

dataset.morphine_subcutaneous_dmd_updated_count = relevant_medications.take(
    relevant_medications.dmd_code.is_in(morphine_subcutaneous_dmd_updated)
).count_for_patient()

dataset.morphine_subcutaneous_multilex_count = relevant_medications.take(
    relevant_medications.multilex_code.is_in(morphine_subcutaneous_multilex)
).count_for_patient()
