from cohortextractor import (codelist_from_csv, combine_codelists)

ethnicity_codes_16 = codelist_from_csv("codelists/opensafely-ethnicity.csv", system = "ctv3", column = "Code", category_column = "Grouping_16",)

# nhs111 = codelist_from_csv("codelists/user-eiliskeeble-nhs-111.csv", system = "snomed", column = "code")

asthma_codes = codelist_from_csv("codelists/opensafely-asthma-diagnosis.csv", system = "ctv3", column = "CTV3ID")

chronic_respiratory_disease_codes = codelist_from_csv("codelists/opensafely-chronic-respiratory-disease.csv",system = "ctv3",column = "CTV3ID",)

chronic_cardiac_disease_codes = codelist_from_csv("codelists/opensafely-chronic-cardiac-disease.csv", system = "ctv3", column = "CTV3ID")

chronic_liver_disease_codes = codelist_from_csv("codelists/opensafely-chronic-liver-disease.csv", system = "ctv3", column = "CTV3ID")

diabetes_codes = codelist_from_csv("codelists/opensafely-diabetes.csv", system = "ctv3", column = "CTV3ID")

dialysis_codes = codelist_from_csv("codelists/opensafely-chronic-kidney-disease.csv", system = "ctv3", column = "CTV3ID")

dementia_codes = codelist_from_csv("codelists/opensafely-dementia.csv", system = "ctv3", column = "CTV3ID")

depression_codes = codelist_from_csv("codelists/opensafely-depression.csv", system = "ctv3", column = "CTV3Code")

hypertension_codes = codelist_from_csv("codelists/opensafely-hypertension.csv", system = "ctv3", column = "CTV3ID")

haem_cancer_codes = codelist_from_csv("codelists/opensafely-haematological-cancer.csv", system = "ctv3", column = "CTV3ID")

lung_cancer_codes = codelist_from_csv("codelists/opensafely-lung-cancer.csv", system = "ctv3", column = "CTV3ID")

inflammatory_bowel_codes = codelist_from_csv("codelists/opensafely-inflammatory-bowel-disease.csv", system = "ctv3", column = "CTV3ID")

other_cancer_codes = codelist_from_csv("codelists/opensafely-cancer-excluding-lung-and-haematological.csv", system = "ctv3", column = "CTV3ID")

other_neuro_codes = codelist_from_csv("codelists/opensafely-other-neurological-conditions.csv", system = "ctv3", column = "CTV3ID")

arthritis_codes = codelist_from_csv("codelists/opensafely-rheumatoid-arthritis.csv", system = "ctv3", column = "CTV3ID")

physical_ltc_codes = combine_codelists(
    chronic_respiratory_disease_codes,
    asthma_codes,
    chronic_cardiac_disease_codes,
    diabetes_codes,
    chronic_liver_disease_codes,
    hypertension_codes,
    dialysis_codes,
    haem_cancer_codes,
    lung_cancer_codes,
    other_cancer_codes,
    inflammatory_bowel_codes,
    arthritis_codes
)

mental_ltc_codes = combine_codelists(
    dementia_codes,
    depression_codes
)

ltc_codes = combine_codelists(
    physical_ltc_codes,
    mental_ltc_codes
)

# sudden_death_codes = codelist_from_csv("codelists/user-eiliskeeble-sudden-death.csv", system = "icd10", column = "icd10")
