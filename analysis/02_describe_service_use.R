################################################################################

########## DESCRIBE SERVICE USE ##########

################################################################################

# Time periods 1 month, 3 months, 1 year
# Quantify service use for patients by cohort (date of death) and place of death separately

# Bivariate comparisons
# Prepandemic - service use variation by place of death
# For each place of death - change in service use pre to phases of pandemic

################################################################################

########## Libraries ##########

library("tidyverse")
library("lubridate")

################################################################################

########## Import data ##########

# Convert dod to date variable
# Create cohort flag
# Death quarter variable starting in March so it is quarters of the cohort period

df_input <- arrow::read_feather(file = here::here("output", "input.feather")) %>%
  mutate(dod_ons = as_date(dod_ons)
        , cohort = case_when(dod_ons >= as_date("2019-03-01") & dod_ons <= as_date("2020-02-29") ~ 0
                              , dod_ons >= as_date("2020-03-01") & dod_ons <= as_date("2021-02-28") ~ 1
                              , TRUE ~ NA_real_)
        , study_quarter = case_when(month(dod_ons) %in% c(3, 4, 5) & year(dod_ons) == 2019 ~ 1
                                    , month(dod_ons) %in% c(6, 7, 8) & year(dod_ons) == 2019 ~ 2
                                    , month(dod_ons) %in% c(9, 10, 11) & year(dod_ons) == 2019 ~ 3
                                    , (month(dod_ons) == 12 & year(dod_ons) == 2019) | (month(dod_ons) %in% c(1, 2) & year(dod_ons) == 2020) ~ 4
                                    , month(dod_ons) %in% c(3, 4, 5) & year(dod_ons) == 2020 ~ 5
                                    , month(dod_ons) %in% c(6, 7, 8) & year(dod_ons) == 2020 ~ 6
                                    , month(dod_ons) %in% c(9, 10, 11) & year(dod_ons) == 2020 ~ 7
                                    , (month(dod_ons) == 12 & year(dod_ons) == 2020) | (month(dod_ons) %in% c(1, 2) & year(dod_ons) == 2021) ~ 8))

################################################################################

########## Descriptive stats service use by cohort ##########

# Just mean currently

service_use_mean_cohort <- df_input %>%
    select(cohort, ends_with("_1yr")) %>%
    pivot_longer(cols = -c(cohort), names_to = "measure", values_to = "value") %>%
    group_by(cohort, measure) %>%
    summarise(mean = mean(value, na.rm = TRUE))

write_csv(service_use_mean_cohort, here::here("output", "service_use_mean_cohort.csv"))

################################################################################

########## Descriptive stats service use by place of death ##########

service_use_mean_pod <- df_input %>%
    select(pod_ons, ends_with("_1yr")) %>%
    pivot_longer(cols = -c(pod_ons), names_to = "measure", values_to = "value") %>%
    group_by(pod_ons, measure) %>%
    summarise(mean = mean(value, na.rm = TRUE))

write_csv(service_use_mean_pod, here::here("output", "service_use_mean_pod.csv"))

################################################################################

########## Ratios of mean service use ##########

# Think about significance testing

service_use_ratio_pod <- df_input %>%
    select(cohort, pod_ons, ends_with("_1yr")) %>%
    pivot_longer(cols = -c(cohort, pod_ons), names_to = "measure", values_to = "value") %>%
    group_by(cohort, pod_ons, measure) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = mean) %>%
    mutate(ratio = cohort_1 / cohort_0)

write_csv(service_use_ratio_pod, here::here("output", "service_use_ratio_pod.csv"))

################################################################################
