# Creates the population needed for the analysis to feed the OS report using ehrQL

# Functions from ehrQL

from ehrql import (Dataset, days, case, when)

from ehrql.tables.beta.tpp import (
    addresses, 
    appointments, 
    clinical_events, 
    emergency_care_attendances, 
    hospital_admissions, 
    medications, 
    ons_deaths,
    opa_diag,
    patients,
    practice_registrations,
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
    practice_registrations.where(practice_registrations.start_date <= dod_ons)
    .except_where(practice_registrations.end_date <= dod_ons)
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
dataset.cod_ons = last_ons_death.underlying_cause_of_death

## Demographics ##

## Sex
dataset.sex = patients.sex

## Age

dataset.age = patients.age_on(dod_ons)

## Ethnicity
dataset.ethnicity = (
    clinical_events.where(
        clinical_events.ctv3_code.is_in(codelists_ehrql.ethnicity_codes_6)
    ).where(
        clinical_events.date.is_on_or_before(dod_ons)
    ).sort_by(
        clinical_events.date
    ).last_for_patient().ctv3_code.to_category(
        codelists_ehrql.ethnicity_codes_6)
)
# No ethnicity from SUS in ehrQL

## Geography ##

## Index of multiple deprivation based on patient address
imd = addresses.for_patient_on("2023-01-01").imd_rounded

dataset.imd_quintile = case(
    when((imd >= 0) & (imd < int(32844 * 1 / 5))).then("1"),
    when(imd < int(32844 * 2 / 5)).then("2"),
    when(imd < int(32844 * 3 / 5)).then("3"),
    when(imd < int(32844 * 4 / 5)).then("4"),
    when(imd < int(32844 * 5 / 5)).then("5"),
    default="0"
)

# Care home resident based on TPP address match
dataset.carehome_tpp = addresses.for_patient_on(dod_ons).care_home_is_potential_match

## Health ##

## Long term conditions
# Check over the five years prior to death

dataset.ltc = clinical_events.where(
    clinical_events.snomedct_code.is_in(codelists_ehrql.ltc_codes)
).where(
    clinical_events.date.is_on_or_between(dod_ons - days(1825), dod_ons)
).exists_for_patient()

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

## Hospital activity

## Elective admissions
dataset.eladm_1m = hospital_admissions.where(
    hospital_admissions.admission_method.is_in(["11", "12", "13"])
).where(
    hospital_admissions.admission_date.is_on_or_between(dod_ons - days(30), dod_ons)
).count_for_patient()

## A&E visits
dataset.aevis_1m = emergency_care_attendances.where(
    emergency_care_attendances.arrival_date.is_on_or_between(dod_ons - days(30), dod_ons)
).count_for_patient()

## Outpatient appointments
# This needs checking - how to do attended appointments?
dataset.opapp_1m = opa_diag.where(
    opa_diag.appointment_date.is_on_or_between(dod_ons - days(30), dod_ons)
).count_for_patient()

## Medications for symptom management at end of life
dataset.eol_med_1m = medications.where(
    medications.dmd_code.is_in(codelists_ehrql.eol_med_codes)
).where(
    medications.date.is_on_or_between(dod_ons - days(30), dod_ons)
).count_for_patient()
