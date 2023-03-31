from databuilder.ehrql import Dataset, codelist_from_csv, months
from databuilder.tables.beta.tpp import (
    medications as m,
    ons_deaths,
    patients,
    practice_registrations as r,
)

# These codelists come from eol_med_codes defined here:
# https://github.com/opensafely/deaths-at-home-covid19/blob/7dd124d4d104a83a3acb17209dc3baa6dfe3da89/analysis/codelists.py#L121-L129
dmd_codelist_names = [
    "glycopyrronium-subcutaneous-formulations",
    "haloperidol-subcutaneous-dmd",
    "hyoscine-butylbromide-subcutaneous-formulations",
    "levomepromazine-subcutaneous",
    "midazolam-end-of-life",
    "morphine-subcutaneous-dmd",
    "oxycodone-subcutaneous-dmd",
]

dmd_codelists = [
    codelist_from_csv(
        f"codelists/opensafely-{name.replace('_', '-')}.csv",
        column="dmd_id",
    )
    for name in dmd_codelist_names
]

# There's no way to combine codelists in ehrQL at the moment, so we do it manually.
dmd_codes = set().union(*(codelist for codelist in dmd_codelists))

# These multilex codes correspond to three items in TPP's medications dictionary for
# "morphine sulfate injection 10mg/1ml" that don't have dm+d codes.
multilex_codes = [
    "14319;1;0",
    "14319;1;2",
    "14319;1;3",
]

# We're interested in events in two periods.
p1_date_range = ("2019-06-01", "2020-02-29")
p2_date_range = ("2020-06-01", "2021-02-28")


dataset = Dataset()

# Record which period each patient died in
date_of_death = ons_deaths.sort_by(ons_deaths.date).last_for_patient().date
dataset.died_in_p1 = date_of_death.is_on_or_between(*p1_date_range)
dataset.died_in_p2 = date_of_death.is_on_or_between(*p2_date_range)

# Set the population.  We're interested in patients:
#  * who died in either of the periods above;
#  * who were registered with a TPP practice when they died;
#  * and whose recorded sex was "female" or "male".
was_registered_at_death = (
    r.where(r.start_date <= date_of_death)
    .except_where(r.end_date <= date_of_death)
    .exists_for_patient()
)
dataset.define_population(
    (dataset.died_in_p1 | dataset.died_in_p2)
    & was_registered_at_death
    & patients.sex.is_in(["female", "male"])
)

dataset.died_at_home = ons_deaths.sort_by(ons_deaths.date).last_for_patient().place == "Home"

# The column counts, for each patient, the number of medication events with a code in
# the dm+d codelist was prescribed in the month before death
dataset.dmd_1 = (
    m.where(m.dmd_code.is_in(dmd_codes))
    .where(m.date.is_on_or_between(date_of_death - months(1), date_of_death))
    .count_for_patient()
)
# The column counts, for each patient, the number of medication events with a code in
# the multilex codelist was prescribed in the month before death
dataset.multilex_1 = (
    m.where(m.multilex_code.is_in(multilex_codes))
    .where(m.date.is_on_or_between(date_of_death - months(1), date_of_death))
    .count_for_patient()
)

# As above, for the three months before death
dataset.dmd_3 = (
    m.where(m.dmd_code.is_in(dmd_codes))
    .where(m.date.is_on_or_between(date_of_death - months(3), date_of_death))
    .count_for_patient()
)
dataset.multilex_3 = (
    m.where(m.multilex_code.is_in(multilex_codes))
    .where(m.date.is_on_or_between(date_of_death - months(3), date_of_death))
    .count_for_patient()
)

# As above, for the twelve months before death
dataset.dmd_12 = (
    m.where(m.dmd_code.is_in(dmd_codes))
    .where(m.date.is_on_or_between(date_of_death - months(12), date_of_death))
    .count_for_patient()
)
dataset.multilex_12 = (
    m.where(m.multilex_code.is_in(multilex_codes))
    .where(m.date.is_on_or_between(date_of_death - months(12), date_of_death))
    .count_for_patient()
)
