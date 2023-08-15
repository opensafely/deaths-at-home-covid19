# Codelists for ehrql study definition

# Functions from ehrql

from ehrql import codelist_from_csv

## DEMOGRAPHICS ##

ethnicity_codes_6 = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    column = "Code",
    category_column = "Grouping_6",
)

## LONG TERM CONDITIONS ##

afib_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-afib_cod.csv",
    column = "code"
)

hf_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-hf_cod.csv",
    column = "code"
)

hyp_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-hyp_cod.csv",
    column = "code"
)

pad_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-pad_cod.csv",
    column = "code"
)

strk_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-strk_cod.csv",
    column = "code"
)

chd_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-chd_cod.csv",
    column = "code"
)

ast_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-ast_cod.csv",
    column = "code"
)

copd_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-copd_cod.csv",
    column = "code"
)

haemcan_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-c19haemcan_cod.csv",
    column = "code"
)

can_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-c19can_cod.csv",
    column = "code"
)

ckd_codes1 = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-ckd_cod.csv",
    column = "code"
)

ckd_codes2 = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-ckd1and2_cod.csv",
    column = "code"
)

dm_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-dm_cod.csv",
    column = "code"
)

rarth_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-rarth_cod.csv",
    column = "code"
)

dem_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-dem_cod.csv",
    column = "code"
)

depr_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-depr_cod.csv",
    column = "code"
)

mh_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-mh_cod.csv",
    column = "code"
)

ld_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-ld_cod.csv",
    column = "code"
)

palcare_codes1 = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-palcare_cod.csv",
    column = "code"
)

palcare_codes2 = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-palcareni_cod.csv",
    column = "code"
)

epil_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-epil_cod.csv",
    column = "code"
) 

osteo_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-osteo_cod.csv",
    column = "code"
) 

ndh_codes = codelist_from_csv(
    "codelists/nhsd-primary-care-domain-refsets-ndh_cod.csv",
    column = "code"
) 

physical_ltc_codes = (
    # Atrial fibrillation
    afib_codes +
    # Coronary heart disease
    chd_codes +
    # Heart failure 
    hf_codes +
    # Hypertension
    hyp_codes +
    # Peripheral arterial disease
    pad_codes +
    # Stroke and transient ischaemic attack
    strk_codes +
    # Asthma
    ast_codes +
    # COPD
    copd_codes +
    # Cancer
    haemcan_codes +
    can_codes +
    # Chronic kidney disease
    ckd_codes1 +
    ckd_codes2 +
    # Diabetes mellitus
    dm_codes +
    # Palliative care
    palcare_codes1 +
    # Epilepsy
    epil_codes +
    # Osteoporosis
    osteo_codes +
    # Rheumatoid arthiritis
    rarth_codes +
    # Non-diabetic hyperglycaemia? 
    ndh_codes
)

mental_ltc_codes = (
    # Dementia
    dem_codes +
    # Depression
    depr_codes +
    # Schizophrenia, Bipolar effective disorder, Psychoses
    mh_codes
)

ltc_codes = (
    physical_ltc_codes +
    mental_ltc_codes +
    ld_codes
)

## MEDICATIONS ##

midazolam_codes = codelist_from_csv(
    "codelists/opensafely-midazolam-end-of-life.csv", 
    column = "dmd_id"
)

glycopyrronium_codes = codelist_from_csv(
    "codelists/opensafely-glycopyrronium-subcutaneous-formulations.csv",
    column = "dmd_id"
)

haloperidol_codes = codelist_from_csv(
    "codelists/opensafely-haloperidol-subcutaneous-dmd.csv",
    column = "dmd_id"
)

hyoscine_butylbromide_codes = codelist_from_csv(
    "codelists/opensafely-hyoscine-butylbromide-subcutaneous-formulations.csv",
    column = "dmd_id"
)

levomepromazine_codes = codelist_from_csv(
    "codelists/opensafely-levomepromazine-subcutaneous.csv",
    column = "dmd_id"
)

morphine_codes = codelist_from_csv(
    "codelists/opensafely-morphine-subcutaneous-dmd.csv",
    column = "dmd_id"
)

oxycodone_codes = codelist_from_csv(
    "codelists/opensafely-oxycodone-subcutaneous-dmd.csv",
    column = "dmd_id"
)

# There's no way to combine codelists in ehrQL at the moment, so we do it manually
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

eol_med_codes = set().union(*(codelist for codelist in dmd_codelists))

## SERVICE USE ##

respite_codes = codelist_from_csv(
    "codelists/user-eiliskeeble-respite-care.csv",
    column = "code"
)

hospice_codes = codelist_from_csv(
    "codelists/user-tgeorghiou-hospice-mentions.csv",
    column = "code"
)

ambulance_codes = codelist_from_csv(
    "codelists/user-eiliskeeble-ambulance-incidents.csv",
    column = "code"
)

community_nursing_codes = codelist_from_csv(
    "codelists/user-eiliskeeble-community-nursing.csv",
    column = "code"
)

mdt_codes = codelist_from_csv(
    "codelists/user-eiliskeeble-multidisciplinary-team.csv",
    column = "code"
)

cancer_mdt_codes = codelist_from_csv(
    "codelists/user-eiliskeeble-cancer-multidisciplinary-team.csv",
    column = "code"
)

community_codes = (
    community_nursing_codes +
    mdt_codes +
    cancer_mdt_codes
)

all_mdt_codes = (
    mdt_codes +
    cancer_mdt_codes
)
