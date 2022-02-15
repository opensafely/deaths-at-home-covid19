################################################################################

########## DESCRIBE SERVICE USE ##########

################################################################################

# Time periods 1 month, 3 months, 1 year
# Quantify service use for patients by cohort (date of death) and place of death separately

# Bivariate comparisons
# Prepandemic - service use variation by place of death
# For each place of death - change in service use pre to phases of pandemic

################################################################################

########## Import data ##########

# Convert dod to date variable
# Create cohort flag
# Death quarter variable starting in March so it is quarters of the cohort period

df_input <- arrow::read_feather(file = here::here("output", "input.feather")) %>%
mutate(dod_ons = as.Date(dod_ons, format = "%Y-%m-%d")
        , cohort = case_when(dod_ons >= as.Date("2019-03-01") & dod_ons <= as.Date("2020-02-29") ~ 0
                            , dod_ons >= as.Date("2020-03-01") & dod_ons <= as.Date("2020-02-28") ~ 1
                            , TRUE ~ NA_integer)
        , dod_quarter = quarter(dod_ons, type = "year.quarter", fiscal_start = 3)
)

################################################################################

########## Descriptive stats service use by cohort ##########

# Just mean currently

service_use_mean_cohort <- df_input %>%
    select(cohort, ends_with("_1yr")) %>%
    pivot_longer(cols = -c(cohort), names_to = "measure", values_to = "value")
    group_by(cohort) %>%
    summarise(mean = mean(value, na.rm = TRUE))

write_csv(service_use_mean_cohort, filename ="service_use_mean_cohort.csv", path = here::here("output"))

################################################################################

########## Descriptive stats service use by place of death ##########

service_use_mean_pod <- df_input %>%
    select(pod_ons, ends_with("_1yr")) %>%
    pivot_longer(cols = -c(cohort), names_to = "measure", values_to = "value")
    group_by(cohort) %>%
    summarise(mean = mean(value, na.rm = TRUE))

write_csv(service_use_mean_pod, filename ="service_use_mean_pod.csv", path = here::here("output"))

################################################################################

########## Ratios of mean service use ##########

# Think about significance testing

service_use_ratio_pod <- df_input %>%
    select(cohort, pod_ons, ends_with("_1yr")) %>%
    pivot_longer(cols = -c(cohort, pod_ons), names_to = "measure", values_to = "value")
    group_by(cohort, pod_ons) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    pivot_wider(names_from = cohort, names_prefix = "cohort", values_from = mean) %>%
    mutate(ratio = cohort_1 / cohort_0)

write_csv(service_use_ratio_pod, filename ="service_use_ratio_pod.csv", path = here::here("output"))

################################################################################
