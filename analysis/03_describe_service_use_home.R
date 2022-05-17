################################################################################

########## DESCRIBE SERVICE USE HOME ##########

################################################################################

# Describe service use for home deaths - focus on inequalities

# Service use for home deaths by cohort and characteristic
# Service use for home deaths by quarter and characteristic

################################################################################

########## Libraries ##########

library("tidyverse")
library("lubridate")

################################################################################

########## Save location ##########

fs::dir_create(here::here("output", "describe_service_use_home"))
fs::dir_create(here::here("output", "describe_service_use_home", "complete_gp_history"))

################################################################################

########## Import data ##########

# Convert dod to date variable
# Create cohort flag
# Death quarter variable starting in March so it is quarters of the cohort period rather than calendar or fiscal quarters
# Join on region and LA imd quintile based on patient address
# Join on LA imd quintile based on GP address

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
                                     , (month(dod_ons) == 12 & year(dod_ons) == 2020) | (month(dod_ons) %in% c(1, 2) & year(dod_ons) == 2021) ~ 8)
         , study_month = floor_date(dod_ons, unit = "month")
         , cod_ons_3 = str_sub(cod_ons, 1, 3)
         , cod_ons_4 = str_sub(cod_ons, 1, 5)
         , pod_ons_new = case_when(pod_ons == "Elsewhere" | pod_ons == "Other communal establishment" ~ "Elsewhere/other"
                                   , TRUE ~ as.character(pod_ons))) %>%
  left_join(read_csv(here::here("docs", "lookups", "msoa_lad_rgn_2020.csv"))
            , by = c("msoa" = "msoa11cd")) %>%
  left_join(read_csv(here::here("docs", "lookups", "lad_imd_2019.csv")) %>% 
              rename(imd_quintile_la = imd19_quintile)
            , by = "lad20cd") %>%
  left_join(read_csv(here::here("docs", "lookups", "msoa_lad_rgn_2020.csv")) %>%
              select(msoa11cd, lad20cd) %>% 
              rename(la_gp = lad20cd)
            , by = c("msoa" = "msoa11cd")) %>%
  left_join(read_csv(here::here("docs", "lookups", "lad_imd_2019.csv")) %>% 
              select(lad20cd, imd19_quintile) %>%
              rename(imd_quintile_la_gp = imd19_quintile)
            , by = "lad20cd") %>%
  mutate(imd_quintile_la = case_when(is.na(imd_quintile_la) ~ 0
                                     , TRUE ~ imd_quintile_la)
         , imd_quintile_la_gp = case_when(is.na(imd_quintile_la_gp) ~ 0
                                          , TRUE ~ imd_quintile_la_gp))

################################################################################

########## Service use of home deaths by cohort and characteristics ##########

# Sex

service_use_cohort_home_sex <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, sex, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, sex), names_to = "measure", values_to = "value") %>%
  group_by(cohort, sex, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), sex, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_sex, here::here("output", "describe_service_use_home", "service_use_cohort_home_sex.csv"))

# Age group

service_use_cohort_home_agegrp <- df_input %>%
  mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "00-09"
                            , age >= 10 & age <= 19 ~ "10-19"
                            , age >= 20 & age <= 29 ~ "20-29"
                            , age >= 30 & age <= 39 ~ "30-39"
                            , age >= 40 & age <= 49 ~ "40-49"
                            , age >= 50 & age <= 59 ~ "50-59"
                            , age >= 60 & age <= 69 ~ "60-69"
                            , age >= 70 & age <= 79 ~ "70-79"
                            , age >= 80 & age <= 89 ~ "80-89"
                            , age >= 90 ~ "90+"
                            , TRUE ~ NA_character_)) %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, agegrp, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, agegrp), names_to = "measure", values_to = "value") %>%
  group_by(cohort, agegrp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), agegrp, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_agegrp, here::here("output", "describe_service_use_home", "service_use_cohort_home_agegrp.csv"))

# Ethnicity

service_use_cohort_home_ethnicity <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, ethnicity, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, ethnicity), names_to = "measure", values_to = "value") %>%
  group_by(cohort, ethnicity, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), ethnicity, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_ethnicity, here::here("output", "describe_service_use_home", "service_use_cohort_home_ethnicity.csv"))

# Long term conditions

service_use_cohort_home_ltc <- df_input %>%
  mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
         , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                               , ltc_count >= 5 ~ "5+"
                               , TRUE ~ NA_character_)) %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, ltc_grp, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, ltc_grp), names_to = "measure", values_to = "value") %>%
  group_by(cohort, ltc_grp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), ltc_grp, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_ltc, here::here("output", "describe_service_use_home", "service_use_cohort_home_ltc.csv"))

# Palliative care

service_use_cohort_home_palcare <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, ltc_palcare1, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, ltc_palcare1), names_to = "measure", values_to = "value") %>%
  group_by(cohort, ltc_palcare1, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), ltc_palcare1, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_palcare, here::here("output", "describe_service_use_home", "service_use_cohort_home_palcare.csv"))

# No palliative care

service_use_cohort_home_nopalcare <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, ltc_palcare2, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, ltc_palcare2), names_to = "measure", values_to = "value") %>%
  group_by(cohort, ltc_palcare2, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), ltc_palcare2, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_nopalcare, here::here("output", "describe_service_use_home", "service_use_cohort_home_nopalcare.csv"))

# Region

service_use_cohort_home_region <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, rgn20cd, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, rgn20cd), names_to = "measure", values_to = "value") %>%
  group_by(cohort, rgn20cd, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), rgn20cd, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_region, here::here("output", "describe_service_use_home", "service_use_cohort_home_region.csv"))

# Deprivation quintile

service_use_cohort_home_imd <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, imd_quintile, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, imd_quintile), names_to = "measure", values_to = "value") %>%
  group_by(cohort, imd_quintile, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), imd_quintile, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_imd, here::here("output", "describe_service_use_home", "service_use_cohort_home_imd.csv"))

# Local authority deprivation quintile

service_use_cohort_home_imd_la <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, imd_quintile_la, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, imd_quintile_la), names_to = "measure", values_to = "value") %>%
  group_by(cohort, imd_quintile_la, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), imd_quintile_la, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_imd_la, here::here("output", "describe_service_use_home", "service_use_cohort_home_imd_la.csv"))

# Rural urban

service_use_cohort_home_rural_urban <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, rural_urban, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, rural_urban), names_to = "measure", values_to = "value") %>%
  group_by(cohort, rural_urban, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), rural_urban, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_rural_urban, here::here("output", "describe_service_use_home", "service_use_cohort_home_rural_urban.csv"))

# GP region

service_use_cohort_home_region_gp <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, region_gp, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, region_gp), names_to = "measure", values_to = "value") %>%
  group_by(cohort, region_gp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), region_gp, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_region_gp, here::here("output", "describe_service_use_home", "service_use_cohort_home_region_gp.csv"))

# GP local authority deprivation quintile

service_use_cohort_home_imd_la_gp <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, imd_quintile_la_gp, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, imd_quintile_la_gp), names_to = "measure", values_to = "value") %>%
  group_by(cohort, imd_quintile_la_gp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), imd_quintile_la_gp, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_imd_la_gp, here::here("output", "describe_service_use_home", "service_use_cohort_home_imd_la_gp.csv"))

# Cause of death

service_use_cohort_home_cod <- df_input %>%
  mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                 , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                 , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                 , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                 , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                 , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                 , TRUE ~ "All other causes")) %>%
  filter(pod_ons_new == "Home") %>% 
  select(cohort, cod_ons_grp, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, cod_ons_grp), names_to = "measure", values_to = "value") %>%
  group_by(cohort, cod_ons_grp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(factor(period, levels = c("1m", "3m", "1y")), cod_ons_grp, activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(service_use_cohort_home_cod, here::here("output", "describe_service_use_home", "service_use_cohort_home_cod.csv"))

##############################

## Calculate the same just for people with complete gp history

# Sex

gp_service_use_cohort_home_sex <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, sex, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, sex), names_to = "measure", values_to = "value") %>%
  group_by(cohort, sex, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, sex, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, sex), names_to = "measure", values_to = "value") %>%
              group_by(cohort, sex, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, sex, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, sex), names_to = "measure", values_to = "value") %>%
              group_by(cohort, sex, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(sex, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_sex, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_sex.csv"))

# Age group

gp_service_use_cohort_home_agegrp <- df_input %>%
  mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "00-09"
                            , age >= 10 & age <= 19 ~ "10-19"
                            , age >= 20 & age <= 29 ~ "20-29"
                            , age >= 30 & age <= 39 ~ "30-39"
                            , age >= 40 & age <= 49 ~ "40-49"
                            , age >= 50 & age <= 59 ~ "50-59"
                            , age >= 60 & age <= 69 ~ "60-69"
                            , age >= 70 & age <= 79 ~ "70-79"
                            , age >= 80 & age <= 89 ~ "80-89"
                            , age >= 90 ~ "90+"
                            , TRUE ~ NA_character_)) %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, agegrp, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, agegrp), names_to = "measure", values_to = "value") %>%
  group_by(cohort, agegrp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "00-09"
                                        , age >= 10 & age <= 19 ~ "10-19"
                                        , age >= 20 & age <= 29 ~ "20-29"
                                        , age >= 30 & age <= 39 ~ "30-39"
                                        , age >= 40 & age <= 49 ~ "40-49"
                                        , age >= 50 & age <= 59 ~ "50-59"
                                        , age >= 60 & age <= 69 ~ "60-69"
                                        , age >= 70 & age <= 79 ~ "70-79"
                                        , age >= 80 & age <= 89 ~ "80-89"
                                        , age >= 90 ~ "90+"
                                        , TRUE ~ NA_character_)) %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, agegrp, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, agegrp), names_to = "measure", values_to = "value") %>%
              group_by(cohort, agegrp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "00-09"
                                        , age >= 10 & age <= 19 ~ "10-19"
                                        , age >= 20 & age <= 29 ~ "20-29"
                                        , age >= 30 & age <= 39 ~ "30-39"
                                        , age >= 40 & age <= 49 ~ "40-49"
                                        , age >= 50 & age <= 59 ~ "50-59"
                                        , age >= 60 & age <= 69 ~ "60-69"
                                        , age >= 70 & age <= 79 ~ "70-79"
                                        , age >= 80 & age <= 89 ~ "80-89"
                                        , age >= 90 ~ "90+"
                                        , TRUE ~ NA_character_)) %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, agegrp, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, agegrp), names_to = "measure", values_to = "value") %>%
              group_by(cohort, agegrp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(agegrp, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_agegrp, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_agegrp.csv"))

# Ethnicity

gp_service_use_cohort_home_ethnicity <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, ethnicity, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, ethnicity), names_to = "measure", values_to = "value") %>%
  group_by(cohort, ethnicity, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, ethnicity, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, ethnicity), names_to = "measure", values_to = "value") %>%
              group_by(cohort, ethnicity, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, ethnicity, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, ethnicity), names_to = "measure", values_to = "value") %>%
              group_by(cohort, ethnicity, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(ethnicity, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_ethnicity, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_ethnicity.csv"))

# Long term conditions

gp_service_use_cohort_home_ltc <- df_input %>%
  mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
         , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                               , ltc_count >= 5 ~ "5+"
                               , TRUE ~ NA_character_)) %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, ltc_grp, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, ltc_grp), names_to = "measure", values_to = "value") %>%
  group_by(cohort, ltc_grp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
                     , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                                           , ltc_count >= 5 ~ "5+"
                                           , TRUE ~ NA_character_)) %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, ltc_grp, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, ltc_grp), names_to = "measure", values_to = "value") %>%
              group_by(cohort, ltc_grp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
                     , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                                           , ltc_count >= 5 ~ "5+"
                                           , TRUE ~ NA_character_)) %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, ltc_grp, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, ltc_grp), names_to = "measure", values_to = "value") %>%
              group_by(cohort, ltc_grp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(ltc_grp, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_ltc, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_ltc.csv"))

# Palliative care

gp_service_use_cohort_home_palcare <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, ltc_palcare1, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, ltc_palcare1), names_to = "measure", values_to = "value") %>%
  group_by(cohort, ltc_palcare1, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, ltc_palcare1, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, ltc_palcare1), names_to = "measure", values_to = "value") %>%
              group_by(cohort, ltc_palcare1, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, ltc_palcare1, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, ltc_palcare1), names_to = "measure", values_to = "value") %>%
              group_by(cohort, ltc_palcare1, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(ltc_palcare1, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_palcare, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_palcare.csv"))

# No palliative care

gp_service_use_cohort_home_nopalcare <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, ltc_palcare2, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, ltc_palcare2), names_to = "measure", values_to = "value") %>%
  group_by(cohort, ltc_palcare2, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, ltc_palcare2, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, ltc_palcare2), names_to = "measure", values_to = "value") %>%
              group_by(cohort, ltc_palcare2, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, ltc_palcare2, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, ltc_palcare2), names_to = "measure", values_to = "value") %>%
              group_by(cohort, ltc_palcare2, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(ltc_palcare2, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_nopalcare, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_nopalcare.csv"))

# Region

gp_service_use_cohort_home_region <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, rgn20cd, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, rgn20cd), names_to = "measure", values_to = "value") %>%
  group_by(cohort, rgn20cd, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, rgn20cd, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, rgn20cd), names_to = "measure", values_to = "value") %>%
              group_by(cohort, rgn20cd, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, rgn20cd, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, rgn20cd), names_to = "measure", values_to = "value") %>%
              group_by(cohort, rgn20cd, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(rgn20cd, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_region, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_region.csv"))

# IMD quintile

gp_service_use_cohort_home_imd <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, imd_quintile, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, imd_quintile), names_to = "measure", values_to = "value") %>%
  group_by(cohort, imd_quintile, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, imd_quintile, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, imd_quintile), names_to = "measure", values_to = "value") %>%
              group_by(cohort, imd_quintile, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, imd_quintile, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, imd_quintile), names_to = "measure", values_to = "value") %>%
              group_by(cohort, imd_quintile, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(imd_quintile, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_imd, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_imd.csv"))

# LA IMD quintile

gp_service_use_cohort_home_imd_la <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, imd_quintile_la, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, imd_quintile_la), names_to = "measure", values_to = "value") %>%
  group_by(cohort, imd_quintile_la, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, imd_quintile_la, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, imd_quintile_la), names_to = "measure", values_to = "value") %>%
              group_by(cohort, imd_quintile_la, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, imd_quintile_la, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, imd_quintile_la), names_to = "measure", values_to = "value") %>%
              group_by(cohort, imd_quintile_la, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(imd_quintile_la, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_imd_la, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_imd_la.csv"))

# Rural urban

gp_service_use_cohort_home_rural_urban <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, rural_urban, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, rural_urban), names_to = "measure", values_to = "value") %>%
  group_by(cohort, rural_urban, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, rural_urban, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, rural_urban), names_to = "measure", values_to = "value") %>%
              group_by(cohort, rural_urban, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, rural_urban, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, rural_urban), names_to = "measure", values_to = "value") %>%
              group_by(cohort, rural_urban, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(rural_urban, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_rural_urban, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_rural_urban.csv"))

# GP region

gp_service_use_cohort_home_region_gp <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, region_gp, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, region_gp), names_to = "measure", values_to = "value") %>%
  group_by(cohort, region_gp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, region_gp, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, region_gp), names_to = "measure", values_to = "value") %>%
              group_by(cohort, region_gp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, region_gp, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, region_gp), names_to = "measure", values_to = "value") %>%
              group_by(cohort, region_gp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(region_gp, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_region_gp, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_region_gp.csv"))

# GP LA IMD quintile

gp_service_use_cohort_home_imd_la_gp <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, imd_quintile_la_gp, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, imd_quintile_la_gp), names_to = "measure", values_to = "value") %>%
  group_by(cohort, imd_quintile_la_gp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, imd_quintile_la_gp, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, imd_quintile_la_gp), names_to = "measure", values_to = "value") %>%
              group_by(cohort, imd_quintile_la_gp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, imd_quintile_la_gp, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, imd_quintile_la_gp), names_to = "measure", values_to = "value") %>%
              group_by(cohort, imd_quintile_la_gp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(imd_quintile_la_gp, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_imd_la_gp, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_imd_la_gp.csv"))

# Cause of death

gp_service_use_cohort_home_cod <- df_input %>%
  mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                  , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                  , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                  , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                  , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                  , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                  , TRUE ~ "All other causes")) %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(cohort, cod_ons_grp, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(cohort, cod_ons_grp), names_to = "measure", values_to = "value") %>%
  group_by(cohort, cod_ons_grp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1)) %>% 
  bind_rows(df_input %>%
              mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                             , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                             , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                             , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                             , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                             , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                             , TRUE ~ "All other causes")) %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, cod_ons_grp, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, cod_ons_grp), names_to = "measure", values_to = "value") %>%
              group_by(cohort, cod_ons_grp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%   
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  bind_rows(df_input %>%
              mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                             , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                             , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                             , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                             , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                             , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                             , TRUE ~ "All other causes")) %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(cohort, cod_ons_grp, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(cohort, cod_ons_grp), names_to = "measure", values_to = "value") %>%
              group_by(cohort, cod_ons_grp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd)) %>%  
              pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = c(n, mean, sd, n_atleast1))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(cod_ons_grp, factor(period, levels = c("1m", "3m", "1y")), activity) %>%
  mutate(mean_ratio = mean_cohort_1/mean_cohort_0)

write_csv(gp_service_use_cohort_home_cod, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_cohort_home_cod.csv"))

################################################################################

##########  Service use of home deaths by quarter and characteristic  ##########

# Sex

service_use_quarter_home_sex <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, sex, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, sex), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, sex, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), sex, activity)

write_csv(service_use_quarter_home_sex, here::here("output", "describe_service_use_home", "service_use_quarter_home_sex.csv"))

# Age group

service_use_quarter_home_agegrp <- df_input %>%
  mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "00-09"
                            , age >= 10 & age <= 19 ~ "10-19"
                            , age >= 20 & age <= 29 ~ "20-29"
                            , age >= 30 & age <= 39 ~ "30-39"
                            , age >= 40 & age <= 49 ~ "40-49"
                            , age >= 50 & age <= 59 ~ "50-59"
                            , age >= 60 & age <= 69 ~ "60-69"
                            , age >= 70 & age <= 79 ~ "70-79"
                            , age >= 80 & age <= 89 ~ "80-89"
                            , age >= 90 ~ "90+"
                            , TRUE ~ NA_character_)) %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, agegrp, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, agegrp), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, agegrp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), agegrp, activity) 

write_csv(service_use_quarter_home_agegrp, here::here("output", "describe_service_use_home", "service_use_quarter_home_agegrp.csv"))

# Ethnicity

service_use_quarter_home_ethnicity <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, ethnicity, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, ethnicity), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, ethnicity, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), ethnicity, activity) 

write_csv(service_use_quarter_home_ethnicity, here::here("output", "describe_service_use_home", "service_use_quarter_home_ethnicity.csv"))

# Long term conditions

service_use_quarter_home_ltc <- df_input %>%
  mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
         , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                               , ltc_count >= 5 ~ "5+"
                               , TRUE ~ NA_character_)) %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, ltc_grp, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, ltc_grp), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, ltc_grp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), ltc_grp, activity) 

write_csv(service_use_quarter_home_ltc, here::here("output", "describe_service_use_home", "service_use_quarter_home_ltc.csv"))

# Palliative care

service_use_quarter_home_palcare <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, ltc_palcare1, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, ltc_palcare1), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, ltc_palcare1, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), ltc_palcare1, activity) 

write_csv(service_use_quarter_home_palcare, here::here("output", "describe_service_use_home", "service_use_quarter_home_palcare.csv"))

# No palliative care

service_use_quarter_home_nopalcare <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, ltc_palcare2, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, ltc_palcare2), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, ltc_palcare2, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), ltc_palcare2, activity) 

write_csv(service_use_quarter_home_nopalcare, here::here("output", "describe_service_use_home", "service_use_quarter_home_nopalcare.csv"))

# Region

service_use_quarter_home_region <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, rgn20cd, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, rgn20cd), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, rgn20cd, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), rgn20cd, activity)

write_csv(service_use_quarter_home_region, here::here("output", "describe_service_use_home", "service_use_quarter_home_region.csv"))

# Deprivation quintile

service_use_quarter_home_imd <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, imd_quintile, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, imd_quintile), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, imd_quintile, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), imd_quintile, activity) 

write_csv(service_use_quarter_home_imd, here::here("output", "describe_service_use_home", "service_use_quarter_home_imd.csv"))

# Local authority deprivation quintile

service_use_quarter_home_imd_la <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, imd_quintile_la, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, imd_quintile_la), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, imd_quintile_la, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), imd_quintile_la, activity) 

write_csv(service_use_quarter_home_imd_la, here::here("output", "describe_service_use_home", "service_use_quarter_home_imd_la.csv"))

# Rural urban

service_use_quarter_home_rural_urban <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, rural_urban, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, rural_urban), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, rural_urban, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), rural_urban, activity) 

write_csv(service_use_quarter_home_rural_urban, here::here("output", "describe_service_use_home", "service_use_quarter_home_rural_urban.csv"))

# GP region

service_use_quarter_home_region_gp <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, region_gp, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, region_gp), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, region_gp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), region_gp, activity) 

write_csv(service_use_quarter_home_region_gp, here::here("output", "describe_service_use_home", "service_use_quarter_home_region_gp.csv"))

# GP local authority deprivation quintile

service_use_quarter_home_imd_la_gp <- df_input %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, imd_quintile_la_gp, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, imd_quintile_la_gp), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, imd_quintile_la_gp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), imd_quintile_la_gp, activity) 

write_csv(service_use_quarter_home_imd_la_gp, here::here("output", "describe_service_use_home", "service_use_quarter_home_imd_la_gp.csv"))

# Cause of death

service_use_quarter_home_cod <- df_input %>%
  mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                 , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                 , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                 , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                 , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                 , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                 , TRUE ~ "All other causes")) %>%
  filter(pod_ons_new == "Home") %>% 
  select(study_quarter, cod_ons_grp, ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, cod_ons_grp), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, cod_ons_grp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(study_quarter, factor(period, levels = c("1m", "3m", "1y")), cod_ons_grp, activity) 

write_csv(service_use_quarter_home_cod, here::here("output", "describe_service_use_home", "service_use_quarter_home_cod.csv"))

##############################

## Calculate the same just for people with complete gp history

# Sex

gp_service_use_quarter_home_sex <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, sex, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, sex), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, sex, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, sex, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, sex), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, sex, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, sex, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, sex), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, sex, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(sex, factor(period, levels = c("1m", "3m", "1y")), activity) 

write_csv(gp_service_use_quarter_home_sex, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_sex.csv"))

# Age group

gp_service_use_quarter_home_agegrp <- df_input %>%
  mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "00-09"
                            , age >= 10 & age <= 19 ~ "10-19"
                            , age >= 20 & age <= 29 ~ "20-29"
                            , age >= 30 & age <= 39 ~ "30-39"
                            , age >= 40 & age <= 49 ~ "40-49"
                            , age >= 50 & age <= 59 ~ "50-59"
                            , age >= 60 & age <= 69 ~ "60-69"
                            , age >= 70 & age <= 79 ~ "70-79"
                            , age >= 80 & age <= 89 ~ "80-89"
                            , age >= 90 ~ "90+"
                            , TRUE ~ NA_character_)) %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, agegrp, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, agegrp), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, agegrp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%
  bind_rows(df_input %>%
              mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "00-09"
                                        , age >= 10 & age <= 19 ~ "10-19"
                                        , age >= 20 & age <= 29 ~ "20-29"
                                        , age >= 30 & age <= 39 ~ "30-39"
                                        , age >= 40 & age <= 49 ~ "40-49"
                                        , age >= 50 & age <= 59 ~ "50-59"
                                        , age >= 60 & age <= 69 ~ "60-69"
                                        , age >= 70 & age <= 79 ~ "70-79"
                                        , age >= 80 & age <= 89 ~ "80-89"
                                        , age >= 90 ~ "90+"
                                        , TRUE ~ NA_character_)) %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, agegrp, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, agegrp), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, agegrp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  bind_rows(df_input %>%
              mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "00-09"
                                        , age >= 10 & age <= 19 ~ "10-19"
                                        , age >= 20 & age <= 29 ~ "20-29"
                                        , age >= 30 & age <= 39 ~ "30-39"
                                        , age >= 40 & age <= 49 ~ "40-49"
                                        , age >= 50 & age <= 59 ~ "50-59"
                                        , age >= 60 & age <= 69 ~ "60-69"
                                        , age >= 70 & age <= 79 ~ "70-79"
                                        , age >= 80 & age <= 89 ~ "80-89"
                                        , age >= 90 ~ "90+"
                                        , TRUE ~ NA_character_)) %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, agegrp, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, agegrp), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, agegrp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(agegrp, factor(period, levels = c("1m", "3m", "1y")), activity) 

write_csv(gp_service_use_quarter_home_agegrp, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_agegrp.csv"))

# Ethnicity

gp_service_use_quarter_home_ethnicity <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, ethnicity, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, ethnicity), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, ethnicity, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, ethnicity, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, ethnicity), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, ethnicity, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, ethnicity, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, ethnicity), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, ethnicity, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(ethnicity, factor(period, levels = c("1m", "3m", "1y")), activity) 

write_csv(gp_service_use_quarter_home_ethnicity, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_ethnicity.csv"))

# Long term conditions

gp_service_use_quarter_home_ltc <- df_input %>%
  mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
         , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                               , ltc_count >= 5 ~ "5+"
                               , TRUE ~ NA_character_)) %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, ltc_grp, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, ltc_grp), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, ltc_grp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>% 
  bind_rows(df_input %>%
              mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
                     , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                                           , ltc_count >= 5 ~ "5+"
                                           , TRUE ~ NA_character_)) %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, ltc_grp, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, ltc_grp), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, ltc_grp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  bind_rows(df_input %>%
              mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
                     , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                                           , ltc_count >= 5 ~ "5+"
                                           , TRUE ~ NA_character_)) %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, ltc_grp, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, ltc_grp), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, ltc_grp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(ltc_grp, factor(period, levels = c("1m", "3m", "1y")), activity)

write_csv(gp_service_use_quarter_home_ltc, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_ltc.csv"))

# Palliative care

gp_service_use_quarter_home_palcare <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, ltc_palcare1, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, ltc_palcare1), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, ltc_palcare1, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%  
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, ltc_palcare1, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, ltc_palcare1), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, ltc_palcare1, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, ltc_palcare1, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, ltc_palcare1), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, ltc_palcare1, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(ltc_palcare1, factor(period, levels = c("1m", "3m", "1y")), activity) 

write_csv(gp_service_use_quarter_home_palcare, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_palcare.csv"))

# No palliative care

gp_service_use_quarter_home_nopalcare <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, ltc_palcare2, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, ltc_palcare2), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, ltc_palcare2, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%  
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, ltc_palcare2, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, ltc_palcare2), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, ltc_palcare2, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, ltc_palcare2, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, ltc_palcare2), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, ltc_palcare2, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(ltc_palcare2, factor(period, levels = c("1m", "3m", "1y")), activity) 

write_csv(gp_service_use_quarter_home_nopalcare, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_nopalcare.csv"))

# Region

gp_service_use_quarter_home_region <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, rgn20cd, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, rgn20cd), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, rgn20cd, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, rgn20cd, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, rgn20cd), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, rgn20cd, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, rgn20cd, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, rgn20cd), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, rgn20cd, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(rgn20cd, factor(period, levels = c("1m", "3m", "1y")), activity) 

write_csv(gp_service_use_quarter_home_region, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_region.csv"))

# IMD quintile

gp_service_use_quarter_home_imd <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, imd_quintile, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, imd_quintile), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, imd_quintile, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%   
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, imd_quintile, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, imd_quintile), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, imd_quintile, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, imd_quintile, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, imd_quintile), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, imd_quintile, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(imd_quintile, factor(period, levels = c("1m", "3m", "1y")), activity) 

write_csv(gp_service_use_quarter_home_imd, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_imd.csv"))

# LA IMD quintile

gp_service_use_quarter_home_imd_la <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, imd_quintile_la, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, imd_quintile_la), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, imd_quintile_la, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%  
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, imd_quintile_la, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, imd_quintile_la), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, imd_quintile_la, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, imd_quintile_la, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, imd_quintile_la), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, imd_quintile_la, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(imd_quintile_la, factor(period, levels = c("1m", "3m", "1y")), activity)

write_csv(gp_service_use_quarter_home_imd_la, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_imd_la.csv"))

# Rural urban

gp_service_use_quarter_home_rural_urban <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, rural_urban, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, rural_urban), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, rural_urban, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%  
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, rural_urban, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, rural_urban), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, rural_urban, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, rural_urban, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, rural_urban), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, rural_urban, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(rural_urban, factor(period, levels = c("1m", "3m", "1y")), activity) 

write_csv(gp_service_use_quarter_home_rural_urban, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_rural_urban.csv"))

# GP region

gp_service_use_quarter_home_region_gp <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, region_gp, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, region_gp), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, region_gp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%  
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, region_gp, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, region_gp), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, region_gp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, region_gp, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, region_gp), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, region_gp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>%  
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(region_gp, factor(period, levels = c("1m", "3m", "1y")), activity) 

write_csv(gp_service_use_quarter_home_region_gp, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_region_gp.csv"))

# GP LA IMD quintile

gp_service_use_quarter_home_imd_la_gp <- df_input %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, imd_quintile_la_gp, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, imd_quintile_la_gp), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, imd_quintile_la_gp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>%  
  bind_rows(df_input %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, imd_quintile_la_gp, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, imd_quintile_la_gp), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, imd_quintile_la_gp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>% 
  bind_rows(df_input %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, imd_quintile_la_gp, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, imd_quintile_la_gp), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, imd_quintile_la_gp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(imd_quintile_la_gp, factor(period, levels = c("1m", "3m", "1y")), activity) 

write_csv(gp_service_use_quarter_home_imd_la_gp, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_imd_la_gp.csv"))

# Cause of death

gp_service_use_quarter_home_cod <- df_input %>%
  mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                 , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                 , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                 , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                 , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                 , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                 , TRUE ~ "All other causes")) %>%
  filter(gp_hist_1m == TRUE & pod_ons_new == "Home") %>% 
  select(study_quarter, cod_ons_grp, ends_with("_1m")) %>%
  select(-contains("gp_hist")) %>% 
  pivot_longer(cols = -c(study_quarter, cod_ons_grp), names_to = "measure", values_to = "value") %>%
  group_by(study_quarter, cod_ons_grp, measure) %>%
  summarise(n = n()
            , mean = mean(value, na.rm = TRUE)
            , sd = sd(value, na.rm = TRUE)
            , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
  mutate(n = plyr::round_any(n, 10)
         , n_atleast1 = plyr::round_any(n_atleast1, 10)
         , mean = case_when(n_atleast1 == 0 ~ 0
                            , TRUE ~ mean)
         , sd = case_when(n_atleast1 == 0 ~ 0
                          , TRUE ~ sd)) %>% 
  bind_rows(df_input %>%
              mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                             , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                             , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                             , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                             , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                             , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                             , TRUE ~ "All other causes")) %>%
              filter(gp_hist_3m == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, cod_ons_grp, ends_with("_3m")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, cod_ons_grp), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, cod_ons_grp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>% 
  bind_rows(df_input %>%
              mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                             , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                             , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                             , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                             , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                             , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                             , TRUE ~ "All other causes")) %>%
              filter(gp_hist_1y == TRUE & pod_ons_new == "Home") %>% 
              select(study_quarter, cod_ons_grp, ends_with("_1y")) %>%
              select(-contains("gp_hist")) %>% 
              pivot_longer(cols = -c(study_quarter, cod_ons_grp), names_to = "measure", values_to = "value") %>%
              group_by(study_quarter, cod_ons_grp, measure) %>%
              summarise(n = n()
                        , mean = mean(value, na.rm = TRUE)
                        , sd = sd(value, na.rm = TRUE)
                        , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
              mutate(n = plyr::round_any(n, 10)
                     , n_atleast1 = plyr::round_any(n_atleast1, 10)
                     , mean = case_when(n_atleast1 == 0 ~ 0
                                        , TRUE ~ mean)
                     , sd = case_when(n_atleast1 == 0 ~ 0
                                      , TRUE ~ sd))) %>% 
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)) %>%
  arrange(cod_ons_grp, factor(period, levels = c("1m", "3m", "1y")), activity)

write_csv(gp_service_use_quarter_home_cod, here::here("output", "describe_service_use_home", "complete_gp_history", "gp_service_use_quarter_home_cod.csv"))

################################################################################
