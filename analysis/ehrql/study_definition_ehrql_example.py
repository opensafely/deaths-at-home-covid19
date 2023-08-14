# Creates the population needed for the analysis to feed the OS report using ehrQL

# Functions from ehrQL

from ehrql import (Dataset, days)

from ehrql.tables.beta.tpp import (
    addresses, 
    appointments, 
    clinical_events, 
    emergency_care_attendances, 
    hospital_admissions, 
    medications as m, 
    ons_deaths,
    patients,
    practice_registrations as r,
)

## CODELISTS ##

# Import codelists from the codelist folder

import codelists_ehrql

## KEY VARIABLES ##

earliest_date = "2019-06-01"
latest_date = "2023-06-30"
date_range = (earliest_date, latest_date)

## STUDY DEFINITION ##

dataset = Dataset()

last_ons_death = ons_deaths.sort_by(ons_deaths.date).last_for_patient()

dod_ons = last_ons_death.date

has_died = dod_ons.is_on_or_between(*date_range)

was_registered_at_death = (
    r.where(r.start_date <= dod_ons)
    .except_where(r.end_date <= dod_ons)
    .exists_for_patient()
)

dataset.define_population(
    has_died
    & was_registered_at_death
    & patients.sex.is_in(["female", "male"])
)

## CREATE VARIABLES ##

## Key cohort variables ##

## ONS date of death
dataset.dod_ons = last_ons_death.date

## ONS place of death
dataset.pod_ons = last_ons_death.place

## ONS cause of death
dataset.cod_ons = last_ons_death.cause_of_death_01

## Demographics ##

## Sex
dataset.sex = patients.sex

## Ethnicity
dataset.latest_ethnicity_code_gp = (
    clinical_events.where(clinical_events.ctv3_code.is_in(codelists_ehrql.ethnicity_codes_6))
    .where(clinical_events.date.is_on_or_before("2023-01-01"))
    .sort_by(clinical_events.date)
    .last_for_patient()
    .ctv3_code
)

dataset.ethnicity_gp = dataset.latest_ethnicity_code_gp.to_category(
    codelists_ehrql.ethnicity_codes_6
)

## Services ##

## GP consultations
dataset.gp_1m = appointments.where(
    appointments.status.is_in([
        "Arrived",
        "In Progress",
        "Finished",
        "Visit",
        "Waiting",
        "Patient Walked Out",
    ])).where(
        appointments.start_date.is_on_or_between(dod_ons - days(30), dod_ons)
    ).count_for_patient()
