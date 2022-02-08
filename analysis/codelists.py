from cohortextractor import (codelist_from_csv, combine_codelists)

ethnicity_codes_6 = codelist_from_csv("codelists/opensafely-ethnicity.csv", system = "ctv3", column = "Code", category_column = "Grouping_6",)

# Will need to revise these/check they align with QOF

af_codes = codelist_from_csv("codelists/opensafely-atrial-fibrillation-or-flutter.csv", system = "ctv3", column = "CTV3Code")

hf_codes = codelist_from_csv("codelists/opensafely-heart-failure.csv", system = "ctv3", column = "CTV3ID")

hypertension_codes = codelist_from_csv("codelists/opensafely-hypertension.csv", system = "ctv3", column = "CTV3ID")

periph_art_codes = codelist_from_csv("codelists/opensafely-peripheral-arterial-disease.csv", system = "ctv3", column = "code")

stroke_codes = codelist_from_csv("codelists/opensafely-stroke-updated.csv", system = "ctv3", column = "CTV3ID")

asthma_codes = codelist_from_csv("codelists/opensafely-asthma-diagnosis.csv", system = "ctv3", column = "CTV3ID")

# QOF COPD available but snomed so can't combine with other codelists
copd_codes = codelist_from_csv("codelists/opensafely-chronic-respiratory-disease.csv", system = "ctv3", column = "CTV3ID",)

haem_cancer_codes = codelist_from_csv("codelists/opensafely-haematological-cancer.csv", system = "ctv3", column = "CTV3ID")

lung_cancer_codes = codelist_from_csv("codelists/opensafely-lung-cancer.csv", system = "ctv3", column = "CTV3ID")

other_cancer_codes = codelist_from_csv("codelists/opensafely-cancer-excluding-lung-and-haematological.csv", system = "ctv3", column = "CTV3ID")

chronic_kidney_disease_codes = codelist_from_csv("codelists/opensafely-chronic-kidney-disease.csv", system = "ctv3", column = "CTV3ID")

diabetes_codes = codelist_from_csv("codelists/opensafely-diabetes.csv", system = "ctv3", column = "CTV3ID")

arthritis_codes = codelist_from_csv("codelists/opensafely-rheumatoid-arthritis.csv", system = "ctv3", column = "CTV3ID")

osteoarthritis_codes = codelist_from_csv("codelists/opensafely-osteoarthritis.csv", system = "ctv3", column = "CTV3ID")

dementia_codes = codelist_from_csv("codelists/opensafely-dementia.csv", system = "ctv3", column = "CTV3ID")

depression_codes = codelist_from_csv("codelists/opensafely-depression.csv", system = "ctv3", column = "CTV3Code")

psych_codes = codelist_from_csv("codelists/opensafely-psychosis-schizophrenia-bipolar-affective-disease.csv", system = "ctv3", column = "CTV3Code")

ld_codes = codelist_from_csv("codelists/opensafely-learning-disabilities.csv", system = "ctv3", column = "CTV3Code")

physical_ltc_codes = combine_codelists(
    # Atrial fibrillation
    af_codes,
    # Coronary heart disease
    # Heart failure 
    hf_codes,
    # Hypertension
    hypertension_codes,
    # Peripheral arterial disease
    periph_art_codes,
    # Stroke and transient ischaemic attack
    stroke_codes,
    # Asthma
    asthma_codes,
    # COPD
    copd_codes,
    # Cancer
    haem_cancer_codes,
    lung_cancer_codes,
    other_cancer_codes,
    # Chronic kidney disease
    chronic_kidney_disease_codes,
    # Diabetes mellitus
    diabetes_codes,
    # Palliative care
    # Epilepsy
    # Osteoporosis
    # Rheumatoid arthiritis
    arthritis_codes,
    osteoarthritis_codes
    # Non-diabetic hyperglycaemia? 
)

mental_ltc_codes = combine_codelists(
    # Dementia
    dementia_codes,
    # Depression
    depression_codes,
    # Schizophrenia, Bipolar effective disorder, Psychoses
    psych_codes,
    # Learning disability?
    ld_codes
)

ltc_codes = combine_codelists(
    physical_ltc_codes,
    mental_ltc_codes
)
