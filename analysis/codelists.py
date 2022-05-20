from dis import code_info
from cohortextractor import (codelist_from_csv, combine_codelists)

## DEMOGRAPHICS ##

ethnicity_codes_6 = codelist_from_csv("codelists/opensafely-ethnicity.csv", system = "ctv3", column = "Code", category_column = "Grouping_6")

## LONG TERM CONDITIONS ##

afib_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-afib_cod.csv", system = "snomed", column = "code")

hf_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-hf_cod.csv", system = "snomed", column = "code")

hyp_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-hyp_cod.csv", system = "snomed", column = "code")

pad_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-pad_cod.csv", system = "snomed", column = "code")

strk_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-strk_cod.csv", system = "snomed", column = "code")

chd_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-chd_cod.csv", system = "snomed", column = "code")

ast_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-ast_cod.csv", system = "snomed", column = "code")

copd_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-copd_cod.csv", system = "snomed", column = "code")

haemcan_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-c19haemcan_cod.csv", system = "snomed", column = "code")

can_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-c19can_cod.csv", system = "snomed", column = "code")

ckd_codes1 = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-ckd_cod.csv", system = "snomed", column = "code")
ckd_codes2 = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-ckd1and2_cod.csv", system = "snomed", column = "code")

dm_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-dm_cod.csv", system = "snomed", column = "code")

rarth_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-rarth_cod.csv", system = "snomed", column = "code")

dem_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-dem_cod.csv", system = "snomed", column = "code")

depr_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-depr_cod.csv", system = "snomed", column = "code")

mh_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-mh_cod.csv", system = "snomed", column = "code")

ld_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-ld_cod.csv", system = "snomed", column = "code")

palcare_codes1 = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-palcare_cod.csv", system = "snomed", column = "code")
palcare_codes2 = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-palcareni_cod.csv", system = "snomed", column = "code")

epil_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-epil_cod.csv", system = "snomed", column = "code") 

osteo_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-osteo_cod.csv", system = "snomed", column = "code") 

ndh_codes = codelist_from_csv("codelists/nhsd-primary-care-domain-refsets-ndh_cod.csv", system = "snomed", column = "code") 

physical_ltc_codes = combine_codelists(
    # Atrial fibrillation
    afib_codes,
    # Coronary heart disease
    chd_codes,
    # Heart failure 
    hf_codes,
    # Hypertension
    hyp_codes,
    # Peripheral arterial disease
    pad_codes,
    # Stroke and transient ischaemic attack
    strk_codes,
    # Asthma
    ast_codes,
    # COPD
    copd_codes,
    # Cancer
    haemcan_codes,
    can_codes,
    # Chronic kidney disease
    ckd_codes1,
    ckd_codes2,
    # Diabetes mellitus
    dm_codes,
    # Palliative care
    palcare_codes1,
    # Epilepsy
    epil_codes,
    # Osteoporosis
    osteo_codes,
    # Rheumatoid arthiritis
    rarth_codes,
    # Non-diabetic hyperglycaemia? 
    ndh_codes
)

mental_ltc_codes = combine_codelists(
    # Dementia
    dem_codes,
    # Depression
    depr_codes,
    # Schizophrenia, Bipolar effective disorder, Psychoses
    mh_codes
)

ltc_codes = combine_codelists(
    physical_ltc_codes,
    mental_ltc_codes,
    ld_codes
)

## MEDICATION ##

midazolam_codes = codelist_from_csv("codelists/opensafely-midazolam-end-of-life.csv", system = "snomed", column = "dmd_id") 

## SERVICE USE ##

respite_codes = codelist_from_csv("codelists/user-eiliskeeble-respite-care.csv", system = "snomed", column = "code")
hospice_codes = codelist_from_csv("codelists/user-tgeorghiou-hospice-mentions.csv", system = "snomed", column = "code")
ambulance_codes = codelist_from_csv("codelists/user-eiliskeeble-ambulance-incidents.csv", system = "snomed", column = "code")
