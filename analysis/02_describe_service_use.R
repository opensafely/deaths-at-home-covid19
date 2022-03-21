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

########## Save location ##########

fs::dir_create(here::here("output", "describe_service_use"))

################################################################################

########## Import data ##########

# Convert dod to date variable
# Create cohort flag
# Death quarter variable starting in March so it is quarters of the cohort period rather than calendar or fiscal quarters

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

# Mean, standard deviation and number of people with at least 1 instance of each activity type

service_use_mean_cohort <- df_input %>%
  select(cohort, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  pivot_longer(cols = -c(cohort), names_to = "measure", values_to = "value") %>%
  group_by(cohort, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_greaterthan1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 5)
         , n_greaterthan1 = plyr::round_any(n_greaterthan1, 5))

write_csv(service_use_mean_cohort, here::here("output", "describe_service_use", "service_use_mean_cohort.csv"))

################################################################################

########## Descriptive stats service use by quarter ##########

service_use_mean_quarter <- df_input %>%
  select(study_quarter, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  pivot_longer(cols = -c(study_quarter), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_greaterthan1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 5)
         , n_greaterthan1 = plyr::round_any(n_greaterthan1, 5))

write_csv(service_use_mean_quarter, here::here("output", "describe_service_use", "service_use_mean_quarter.csv"))

################################################################################

########## Descriptive stats service use by cohort and place of death ##########

service_use_mean_cohort_pod <- df_input %>%
    select(cohort, pod_ons, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
    pivot_longer(cols = -c(cohort, pod_ons), names_to = "measure", values_to = "value") %>%
    group_by(cohort, pod_ons, measure) %>%
    summarise(n = n()
              , mean = mean(value, na.rm = TRUE)
              , sd = sd(value, na.rm = TRUE)
              , n_greaterthan1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 5)
         , n_greaterthan1 = plyr::round_any(n_greaterthan1, 5))

write_csv(service_use_mean_cohort_pod, here::here("output", "describe_service_use", "service_use_mean_cohort_pod.csv"))

################################################################################

########## Descriptive stats service use by study quarter and place of death ##########

service_use_mean_quarter_pod <- df_input %>%
  select(study_quarter, pod_ons, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  pivot_longer(cols = -c(study_quarter, pod_ons), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, pod_ons, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_greaterthan1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 5)
         , n_greaterthan1 = plyr::round_any(n_greaterthan1, 5))

write_csv(service_use_mean_quarter_pod, here::here("output", "describe_service_use", "service_use_mean_quarter_pod.csv"))

################################################################################

########## Ratios of mean service use ##########

# Think about any significance testing

# Ratio by cohort

service_use_ratio <- df_input %>%
  select(cohort, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  pivot_longer(cols = -c(cohort), names_to = "measure", values_to = "value") %>%
  group_by(cohort, measure) %>%
  summarise(mean = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = mean) %>%
  mutate(ratio = cohort_1 / cohort_0)

write_csv(service_use_ratio, here::here("output", "describe_service_use", "service_use_ratio.csv"))

# Ratio for each place of death by cohort

service_use_ratio_pod <- df_input %>%
    select(cohort, pod_ons, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
    pivot_longer(cols = -c(cohort, pod_ons), names_to = "measure", values_to = "value") %>%
    group_by(cohort, pod_ons, measure) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = mean) %>%
    mutate(ratio = cohort_1 / cohort_0)

write_csv(service_use_ratio_pod, here::here("output", "describe_service_use", "service_use_ratio_pod.csv"))

# Ratio for each place of death pre-pandemic quarter to pandemic quarter e.g. Mar-May 19 to Mar-May 20

service_use_ratio_pod_quarter <- df_input %>%
  select(study_quarter, pod_ons, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  pivot_longer(cols = -c(study_quarter, pod_ons), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, pod_ons, measure) %>%
  summarise(mean = mean(value, na.rm = TRUE)) %>%
  pivot_wider(names_from = study_quarter, names_prefix = "quarter_", values_from = mean) %>%
  mutate(ratio_q1 = quarter_5 - quarter_1
         , ratio_q2 = quarter_6 - quarter_2
         , ratio_q3 = quarter_7 - quarter_3
         , ratio_q4 = quarter_8 - quarter_4)

write_csv(service_use_ratio_pod_quarter, here::here("output", "describe_service_use", "service_use_ratio_pod_quarter.csv"))

################################################################################