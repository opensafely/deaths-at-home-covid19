################################################################################

########## SERVICE USE MODELS ##########

################################################################################

# Model service use by cohort
# - Poisson model of means
# - Binomial models of proportions
# Model service use by cohort and place of death
# - Poisson and binomial
# - Interaction between cohort and place of death

# Just for final service use types

################################################################################

########## Libraries ##########

library("tidyverse")
library("lubridate")

################################################################################

########## Save location ##########

fs::dir_create(here::here("output", "describe_service_use"))
fs::dir_create(here::here("output", "describe_service_use", "models"))
fs::dir_create(here::here("output", "describe_service_use", "complete_gp_history"))
fs::dir_create(here::here("output", "describe_service_use", "complete_gp_history", "models"))

################################################################################

########## Import data ##########

# Convert dod to date variable
# Create cohort flag and additional flag to identify deaths in our revised 9 month cohort period
# Death quarter variable starting in March so it is quarters of the cohort period rather than calendar or fiscal quarters
# Create grouped and renamed characteristic variables
# Join on region and LA imd quintile based on patient address
# Join on LA imd quintile based on GP address

df_input <- arrow::read_feather(file = here::here("output", "input.feather")) %>%
  mutate(dod_ons = as_date(dod_ons)
         , cohort = case_when(dod_ons >= as_date("2019-03-01") & dod_ons <= as_date("2020-02-29") ~ 0
                              , dod_ons >= as_date("2020-03-01") & dod_ons <= as_date("2021-02-28") ~ 1
                              , TRUE ~ NA_real_)
         , study_cohort = case_when(dod_ons >= as_date("2019-06-01") & dod_ons <= as_date("2020-02-29") ~ 0
                                    , dod_ons >= as_date("2020-06-01") & dod_ons <= as_date("2021-02-28") ~ 1
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
                                   , TRUE ~ as.character(pod_ons))
         , agegrp = case_when(age >= 0 & age <= 09 ~ "00-09"
                              , age >= 10 & age <= 19 ~ "10-19"
                              , age >= 20 & age <= 29 ~ "20-29"
                              , age >= 30 & age <= 39 ~ "30-39"
                              , age >= 40 & age <= 49 ~ "40-49"
                              , age >= 50 & age <= 59 ~ "50-59"
                              , age >= 60 & age <= 69 ~ "60-69"
                              , age >= 70 & age <= 79 ~ "70-79"
                              , age >= 80 & age <= 89 ~ "80-89"
                              , age >= 90 ~ "90+"
                              , TRUE ~ NA_character_)
         , ltc_count = arrow::read_feather(file = here::here("output", "input.feather")) %>% select(starts_with("ltc_")) %>% rowSums()
         , ltcgrp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                              , ltc_count >= 5 ~ "5+"
                              , TRUE ~ NA_character_)
         , codgrp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                              , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                              , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                              , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                              , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                              , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                              , TRUE ~ "All other causes")
         , palcare = ltc_palcare1
         , nopalcare = ltc_palcare2
         , rural_urban = case_when(rural_class %in% c(1, 2, 3, 4)  ~ "Urban"
                                   , rural_class %in% c(5, 6, 7, 8) ~ "Rural"
                                   , TRUE ~ NA_character_)) %>%
  left_join(read_csv(here::here("docs", "lookups", "msoa_lad_rgn_2020.csv")) %>% 
              select(msoa11cd, lad20cd, rgn20cd) %>% 
              rename(region = rgn20cd)
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

########## Model service use by cohort ##########

measure <- colnames(df_input %>%
                      select(starts_with(c("aevis", "community", "eladm", "elbeddays", "emadm", "embeddays", "eol_med", "gp", "palliative", "opapp", "opatt"))))

# Test differences in means with poisson model

model_service_use_mean_cohort <- tibble(measure = measure) %>%
  filter(!str_detect(measure, "^gp_hist")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map(measure, function(var) df_input %>%
                         filter(!is.na(study_cohort)) %>% 
                         select(cohort, all_of(var)) %>%
                         pivot_longer(cols = -c(cohort), names_to = "measure", values_to = "value") %>%
                         mutate(activity = str_sub(measure, 1, -4)
                                , period = str_sub(measure, -2, -1)
                                , cohort = as_factor(cohort)))
         , model = map(dataset, function(dset) glm(value ~ cohort, data = dset, family = poisson(link = "log")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , cohort_0         = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0), 1)))$estimate), 3))
         , cohort_1         = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1), 1)))$estimate), 3))
  ) %>%
  select(-dataset, -model)

write_csv(model_service_use_mean_cohort, here::here("output", "describe_service_use", "models", "model_service_use_mean_cohort.csv"))

# Test differences proportion with at least 1 event with binomial model and identity link

model_service_use_prop_cohort <- tibble(measure = measure) %>%
  filter(!str_detect(measure, "^gp_hist")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map(measure, function(var) df_input %>%
                         filter(!is.na(study_cohort)) %>% 
                         select(cohort, all_of(var)) %>%
                         pivot_longer(cols = -c(cohort), names_to = "measure", values_to = "value") %>%
                         mutate(activity = str_sub(measure, 1, -4)
                                , period = str_sub(measure, -2, -1)
                                , cohort = as_factor(cohort)
                                , n_atleast1 = case_when(value >= 1 ~ 1
                                                         , TRUE ~ 0)))
         , model = map(dataset, function(dset) glm(n_atleast1 ~ cohort, data = dset, family = binomial(link = "identity")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , cohort_0         = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0), 1)))$estimate, 3))
         , cohort_1         = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1), 1)))$estimate, 3))
  ) %>%
  select(-dataset, -model)

write_csv(model_service_use_prop_cohort, here::here("output", "describe_service_use", "models", "model_service_use_prop_cohort.csv"))

# Test differences proportion with at least 3 emergency admissions in 3 months with binomial model and identity link

model_emadm3_prop_cohort <- tibble(measure = c("emadm_3m")) %>%
  filter(!str_detect(measure, "^gp_hist")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map(measure, function(var) df_input %>%
                         filter(!is.na(study_cohort)) %>% 
                         select(cohort, all_of(var)) %>%
                         pivot_longer(cols = -c(cohort), names_to = "measure", values_to = "value") %>%
                         mutate(activity = str_sub(measure, 1, -4)
                                , period = str_sub(measure, -2, -1)
                                , cohort = as_factor(cohort)
                                , n_atleast3 = case_when(value >= 1 ~ 1
                                                         , TRUE ~ 0)))
         , model = map(dataset, function(dset) glm(n_atleast3 ~ cohort, data = dset, family = binomial(link = "identity")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , cohort_0         = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0), 1)))$estimate, 3))
         , cohort_1         = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1), 1)))$estimate, 3))
  ) %>%
  select(-dataset, -model)

write_csv(model_emadm3_prop_cohort, here::here("output", "describe_service_use", "models", "model_emadm3_prop_cohort.csv"))

##############################

# Calculate the same just for people with complete gp history

# Test differences in activity counts with poisson model

gp_model_service_use_mean_cohort <- tibble(measure = measure) %>%
  filter(!str_detect(measure, "^gp_hist")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map2(activity, period, function(var, time) df_input %>%
                            filter(!is.na(study_cohort)) %>% 
                            select(cohort, starts_with(var), starts_with("gp_hist")) %>%
                            select(cohort, ends_with(time)) %>%
                            rename(gp_hist = paste0("gp_hist_", time)
                                   , value = paste0(var, "_", time)) %>%
                            mutate(activity = all_of(var)
                                   , period = all_of(time)
                                   , cohort = as_factor(cohort)) %>%
                            filter(gp_hist == 1))
         , model = map(dataset, function(dset) glm(value ~ cohort, data = dset, family = poisson(link = "log")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , cohort_0         = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0), 1)))$estimate), 3))
         , cohort_1         = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1), 1)))$estimate), 3))
  ) %>%
  select(-dataset, -model)

write_csv(gp_model_service_use_mean_cohort, here::here("output", "describe_service_use", "complete_gp_history", "models", "gp_model_service_use_mean_cohort.csv"))

# Test differences proportion with at least 1 event with binomial model and identity link

gp_model_service_use_prop_cohort <- tibble(measure = measure) %>%
  filter(!str_detect(measure, "^gp_hist")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map2(activity, period, function(var, time) df_input %>%
                            filter(!is.na(study_cohort)) %>% 
                            select(cohort, starts_with(var), starts_with("gp_hist")) %>%
                            select(cohort, ends_with(time)) %>%
                            rename(gp_hist = paste0("gp_hist_", time)
                                   , value = paste0(var, "_", time)) %>%
                            mutate(activity = all_of(var)
                                   , period = all_of(time)
                                   , cohort = as_factor(cohort)
                                   , n_atleast1 = case_when(value >= 1 ~ 1
                                                            , TRUE ~ 0)) %>%
                            filter(gp_hist == 1))
         , model = map(dataset, function(dset) glm(n_atleast1 ~ cohort, data = dset, family = binomial(link = "identity")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , cohort_0         = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0), 1)))$estimate, 3))
         , cohort_1         = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1), 1)))$estimate, 3))
  ) %>%
  select(-dataset, -model)

write_csv(gp_model_service_use_prop_cohort, here::here("output", "describe_service_use", "complete_gp_history", "models", "gp_model_service_use_prop_cohort.csv"))

# Test differences proportion with at least 3 emergency admissions with binomial model and identity link

gp_model_emadm3_prop_cohort <- tibble(measure = c("emadm_3m")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map2(activity, period, function(var, time) df_input %>%
                            filter(!is.na(study_cohort)) %>% 
                            select(cohort, starts_with(var), starts_with("gp_hist")) %>%
                            select(cohort, ends_with(time)) %>%
                            rename(gp_hist = paste0("gp_hist_", time)
                                   , value = paste0(var, "_", time)) %>%
                            mutate(activity = all_of(var)
                                   , period = all_of(time)
                                   , cohort = as_factor(cohort)
                                   , n_atleast3 = case_when(value >= 3 ~ 1
                                                            , TRUE ~ 0)) %>%
                            filter(gp_hist == 1))
         , model = map(dataset, function(dset) glm(n_atleast3 ~ cohort, data = dset, family = binomial(link = "identity")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , cohort_0         = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0), 1)))$estimate, 3))
         , cohort_1         = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1), 1)))$estimate, 3))
  ) %>%
  select(-dataset, -model)

write_csv(gp_model_emadm3_prop_cohort, here::here("output", "describe_service_use", "complete_gp_history", "models", "gp_model_emadm3_prop_cohort.csv"))

################################################################################

########## Model service use by cohort and place of death ##########

# Test differences in means with poisson model

model_service_use_mean_cohort_pod <- tibble(measure = measure) %>%
  filter(!str_detect(measure, "^gp_hist")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map(measure,  function(var) df_input %>%
                           filter(!is.na(study_cohort)) %>% 
                           select(cohort, pod_ons_new, all_of(var)) %>%
                           rename(value = var) %>%
                           mutate(activity = str_sub(var, 1, -4)
                                  , period = str_sub(var, -2, -1)
                                  , cohort = as_factor(cohort)
                                  , pod_ons_new = factor(pod_ons_new, levels = c("Home", "Care home", "Hospital"
                                                                                 , "Hospice", "Elsewhere/other"))))     
         , model = map(dataset, function(dset) glm(value ~ cohort*pod_ons_new, data = dset
                                                   , family = poisson(link = "log")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , carehome_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[3], 4))
         , carehome_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[3], 4))
         , carehome_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[3], 4))
         , hospital_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[4], 4))
         , hospital_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[4], 4))
         , hospital_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[4], 4))
         , hospice_coeff    = map_dbl(model, function(x) round(broom::tidy(x)$estimate[5], 4))
         , hospice_se       = map_dbl(model, function(x) round(broom::tidy(x)$std.error[5], 4))
         , hospice_pvalue   = map_dbl(model, function(x) round(broom::tidy(x)$p.value[5], 4))
         , other_coeff      = map_dbl(model, function(x) round(broom::tidy(x)$estimate[6], 4))
         , other_se         = map_dbl(model, function(x) round(broom::tidy(x)$std.error[6], 4))
         , other_pvalue     = map_dbl(model, function(x) round(broom::tidy(x)$p.value[6], 4))
         , interact_carehome_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[7], 4))
         , interact_carehome_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[7], 4))
         , interact_carehome_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[7], 4))
         , interact_hospital_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[8], 4))
         , interact_hospital_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[8], 4))
         , interact_hospital_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[8], 4))
         , interact_hospice_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[9], 4))
         , interact_hospice_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[9], 4))
         , interact_hospice_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[9], 4))
         , interact_other_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[10], 4))
         , interact_other_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[10], 4))
         , interact_other_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[10], 4))
         , interact_chisq_pvalue    = map_dbl(model, function(x) round(anova(x, test = "Chisq")$`Pr(>Chi)`[4], 4))
         , home_cohort_0     = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate), 3))
         , home_cohort_1     = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate), 3))
         , carehome_cohort_0 = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate), 3))
         , carehome_cohort_1 = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 1, 0, 0, 0, 1, 0, 0, 0), 1)))$estimate), 3))
         , hospital_cohort_0 = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0), 1)))$estimate), 3))
         , hospital_cohort_1 = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 1, 0, 0, 0, 1, 0, 0), 1)))$estimate), 3))
         , hospice_cohort_0  = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0), 1)))$estimate), 3))
         , hospice_cohort_1  = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 1, 0, 0, 0, 1, 0), 1)))$estimate), 3))
         , other_cohort_0    = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0), 1)))$estimate), 3))
         , other_cohort_1    = map_dbl(model, function(x) round(exp(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 1, 0, 0, 0, 1), 1)))$estimate), 3))
         , home_cohort_0_1_pvalue     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , carehome_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , hospital_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , hospice_cohort_0_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , other_cohort_0_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, -1), 1)))$adj.p.value, 4))
         , home_carehome_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, -1, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, -1, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_0_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, -1, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_0_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, 0, -1, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_carehome_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, -1, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, -1, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, -1, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, 0, -1, 0, 0, 0, -1), 1)))$adj.p.value, 4))
  ) %>%
  select(-dataset, -model)

write_csv(model_service_use_mean_cohort_pod, here::here("output", "describe_service_use", "models", "model_service_use_mean_cohort_pod.csv"))

# Test differences proportion with at least 1 event with binomial model and identity link

model_service_use_prop_cohort_pod <- tibble(measure = measure) %>%
  filter(!str_detect(measure, "^gp_hist")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map(measure, function(var) df_input %>%
                           filter(!is.na(study_cohort)) %>% 
                           select(cohort, pod_ons_new, all_of(var)) %>%
                           rename(value = var) %>%
                           mutate(activity = str_sub(var, 1, -4)
                                  , period = str_sub(var, -2, -1)
                                  , cohort = as_factor(cohort)
                                  , n_atleast1 = case_when(value >= 1 ~ 1
                                                           , TRUE ~ 0)
                                  , pod_ons_new = factor(pod_ons_new
                                                         , levels = c("Home", "Care home", "Hospital"
                                                                      , "Hospice", "Elsewhere/other"))))   
         , model = map(dataset, function(dset) glm(n_atleast1 ~ cohort*pod_ons_new, data = dset, family = binomial(link = "identity")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , carehome_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[3], 4))
         , carehome_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[3], 4))
         , carehome_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[3], 4))
         , hospital_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[4], 4))
         , hospital_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[4], 4))
         , hospital_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[4], 4))
         , hospice_coeff    = map_dbl(model, function(x) round(broom::tidy(x)$estimate[5], 4))
         , hospice_se       = map_dbl(model, function(x) round(broom::tidy(x)$std.error[5], 4))
         , hospice_pvalue   = map_dbl(model, function(x) round(broom::tidy(x)$p.value[5], 4))
         , other_coeff      = map_dbl(model, function(x) round(broom::tidy(x)$estimate[6], 4))
         , other_se         = map_dbl(model, function(x) round(broom::tidy(x)$std.error[6], 4))
         , other_pvalue     = map_dbl(model, function(x) round(broom::tidy(x)$p.value[6], 4))
         , interact_carehome_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[7], 4))
         , interact_carehome_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[7], 4))
         , interact_carehome_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[7], 4))
         , interact_hospital_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[8], 4))
         , interact_hospital_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[8], 4))
         , interact_hospital_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[8], 4))
         , interact_hospice_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[9], 4))
         , interact_hospice_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[9], 4))
         , interact_hospice_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[9], 4))
         , interact_other_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[10], 4))
         , interact_other_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[10], 4))
         , interact_other_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[10], 4))
         , interact_chisq_pvalue    = map_dbl(model, function(x) round(anova(x, test = "Chisq")$`Pr(>Chi)`[4], 4))
         , home_cohort_0     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , home_cohort_1     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , carehome_cohort_0 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , carehome_cohort_1 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 1, 0, 0, 0, 1, 0, 0, 0), 1)))$estimate, 3))
         , hospital_cohort_0 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , hospital_cohort_1 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 1, 0, 0, 0, 1, 0, 0), 1)))$estimate, 3))
         , hospice_cohort_0  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , hospice_cohort_1  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 1, 0, 0, 0, 1, 0), 1)))$estimate, 3))
         , other_cohort_0    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0), 1)))$estimate, 3))
         , other_cohort_1    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 1, 0, 0, 0, 1), 1)))$estimate, 3))
         , home_cohort_0_1_pvalue     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , carehome_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , hospital_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , hospice_cohort_0_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , other_cohort_0_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, -1), 1)))$adj.p.value, 4))
         , home_carehome_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, -1, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, -1, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_0_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, -1, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_0_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, 0, -1, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_carehome_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, -1, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, -1, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, -1, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, 0, -1, 0, 0, 0, -1), 1)))$adj.p.value, 4))
  ) %>%
  select(-dataset, -model)

write_csv(model_service_use_prop_cohort_pod, here::here("output", "describe_service_use", "models", "model_service_use_prop_cohort_pod.csv"))

# Test differences proportion with at least 3 emergency admissions  with binomial model and identity link

model_emadm3_prop_cohort_pod <- tibble(measure = c("emadm_1m", "emadm_3m", "emadm_1y")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map(measure, function(var) df_input %>%
                           filter(!is.na(study_cohort)) %>% 
                           select(cohort, pod_ons_new, all_of(var)) %>%
                           rename(value = var) %>%
                           mutate(activity = str_sub(var, 1, -4)
                                  , period = str_sub(var, -2, -1)
                                  , cohort = as_factor(cohort)
                                  , n_atleast3 = case_when(value >= 3 ~ 1
                                                           , TRUE ~ 0)
                                  , pod_ons_new = factor(pod_ons_new
                                                         , levels = c("Home", "Care home", "Hospital"
                                                                      , "Hospice", "Elsewhere/other"))))   
         , model = map(dataset, function(dset) glm(n_atleast3 ~ cohort*pod_ons_new, data = dset, family = binomial(link = "identity")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , carehome_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[3], 4))
         , carehome_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[3], 4))
         , carehome_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[3], 4))
         , hospital_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[4], 4))
         , hospital_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[4], 4))
         , hospital_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[4], 4))
         , hospice_coeff    = map_dbl(model, function(x) round(broom::tidy(x)$estimate[5], 4))
         , hospice_se       = map_dbl(model, function(x) round(broom::tidy(x)$std.error[5], 4))
         , hospice_pvalue   = map_dbl(model, function(x) round(broom::tidy(x)$p.value[5], 4))
         , other_coeff      = map_dbl(model, function(x) round(broom::tidy(x)$estimate[6], 4))
         , other_se         = map_dbl(model, function(x) round(broom::tidy(x)$std.error[6], 4))
         , other_pvalue     = map_dbl(model, function(x) round(broom::tidy(x)$p.value[6], 4))
         , interact_carehome_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[7], 4))
         , interact_carehome_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[7], 4))
         , interact_carehome_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[7], 4))
         , interact_hospital_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[8], 4))
         , interact_hospital_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[8], 4))
         , interact_hospital_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[8], 4))
         , interact_hospice_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[9], 4))
         , interact_hospice_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[9], 4))
         , interact_hospice_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[9], 4))
         , interact_other_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[10], 4))
         , interact_other_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[10], 4))
         , interact_other_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[10], 4))
         , interact_chisq_pvalue    = map_dbl(model, function(x) round(anova(x, test = "Chisq")$`Pr(>Chi)`[4], 4))
         , home_cohort_0     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , home_cohort_1     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , carehome_cohort_0 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , carehome_cohort_1 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 1, 0, 0, 0, 1, 0, 0, 0), 1)))$estimate, 3))
         , hospital_cohort_0 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , hospital_cohort_1 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 1, 0, 0, 0, 1, 0, 0), 1)))$estimate, 3))
         , hospice_cohort_0  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , hospice_cohort_1  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 1, 0, 0, 0, 1, 0), 1)))$estimate, 3))
         , other_cohort_0    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0), 1)))$estimate, 3))
         , other_cohort_1    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 1, 0, 0, 0, 1), 1)))$estimate, 3))
         , home_cohort_0_1_pvalue     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , carehome_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , hospital_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , hospice_cohort_0_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , other_cohort_0_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, -1), 1)))$adj.p.value, 4))
         , home_carehome_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, -1, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, -1, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_0_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, -1, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_0_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, 0, -1, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_carehome_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, -1, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, -1, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, -1, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, 0, -1, 0, 0, 0, -1), 1)))$adj.p.value, 4))  ) %>%
  select(-dataset, -model)

write_csv(model_emadm3_prop_cohort_pod, here::here("output", "describe_service_use", "models", "model_emadm3_prop_cohort_pod.csv"))

##############################

# Calculate the same just for people with complete gp history

# Test differences in means with poisson model

gp_model_service_use_mean_cohort_pod <- tibble(measure = measure) %>%
  filter(!str_detect(measure, "^gp_hist")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map2(activity, period,  function(var, time) df_input %>%
                            filter(!is.na(study_cohort)) %>% 
                            select(cohort, pod_ons_new, starts_with(var), starts_with("gp_hist")) %>%
                            select(cohort, pod_ons_new, ends_with(time)) %>%
                            rename(gp_hist = paste0("gp_hist_", time)
                                   , value = paste0(var, "_", time)) %>%
                            mutate(activity = all_of(var)
                                   , period = all_of(time)
                                   , cohort = as_factor(cohort)
                                   , pod_ons_new = factor(pod_ons_new, levels = c("Home", "Care home", "Hospital"
                                                                                  , "Hospice", "Elsewhere/other"))) %>%
                            filter(gp_hist == 1))         
         , model = map(dataset, function(dset) glm(value ~ cohort*pod_ons_new, data = dset
                                                   , family = poisson(link = "log")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , carehome_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[3], 4))
         , carehome_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[3], 4))
         , carehome_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[3], 4))
         , hospital_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[4], 4))
         , hospital_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[4], 4))
         , hospital_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[4], 4))
         , hospice_coeff    = map_dbl(model, function(x) round(broom::tidy(x)$estimate[5], 4))
         , hospice_se       = map_dbl(model, function(x) round(broom::tidy(x)$std.error[5], 4))
         , hospice_pvalue   = map_dbl(model, function(x) round(broom::tidy(x)$p.value[5], 4))
         , other_coeff      = map_dbl(model, function(x) round(broom::tidy(x)$estimate[6], 4))
         , other_se         = map_dbl(model, function(x) round(broom::tidy(x)$std.error[6], 4))
         , other_pvalue     = map_dbl(model, function(x) round(broom::tidy(x)$p.value[6], 4))
         , interact_carehome_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[7], 4))
         , interact_carehome_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[7], 4))
         , interact_carehome_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[7], 4))
         , interact_hospital_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[8], 4))
         , interact_hospital_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[8], 4))
         , interact_hospital_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[8], 4))
         , interact_hospice_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[9], 4))
         , interact_hospice_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[9], 4))
         , interact_hospice_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[9], 4))
         , interact_other_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[10], 4))
         , interact_other_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[10], 4))
         , interact_other_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[10], 4))
         , interact_chisq_pvalue    = map_dbl(model, function(x) round(anova(x, test = "Chisq")$`Pr(>Chi)`[4], 4))
         , home_cohort_0     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , home_cohort_1     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , carehome_cohort_0 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , carehome_cohort_1 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 1, 0, 0, 0, 1, 0, 0, 0), 1)))$estimate, 3))
         , hospital_cohort_0 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , hospital_cohort_1 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 1, 0, 0, 0, 1, 0, 0), 1)))$estimate, 3))
         , hospice_cohort_0  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , hospice_cohort_1  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 1, 0, 0, 0, 1, 0), 1)))$estimate, 3))
         , other_cohort_0    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0), 1)))$estimate, 3))
         , other_cohort_1    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 1, 0, 0, 0, 1), 1)))$estimate, 3))
         , home_cohort_0_1_pvalue     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , carehome_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , hospital_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , hospice_cohort_0_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , other_cohort_0_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, -1), 1)))$adj.p.value, 4))
         , home_carehome_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, -1, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, -1, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_0_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, -1, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_0_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, 0, -1, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_carehome_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, -1, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, -1, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, -1, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, 0, -1, 0, 0, 0, -1), 1)))$adj.p.value, 4))  ) %>%
  select(-dataset, -model)

write_csv(gp_model_service_use_mean_cohort_pod, here::here("output", "describe_service_use", "complete_gp_history", "models", "gp_model_service_use_mean_cohort_pod.csv"))

# Test differences proportion with at least 1 event with binomial model and identity link

gp_model_service_use_prop_cohort_pod <- tibble(measure = measure) %>%
  filter(!str_detect(measure, "^gp_hist")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map2(activity, period,  function(var, time) df_input %>%
                            filter(!is.na(study_cohort)) %>% 
                            select(cohort, pod_ons_new, starts_with(var), starts_with("gp_hist")) %>%
                            select(cohort, pod_ons_new, ends_with(time)) %>%
                            rename(gp_hist = paste0("gp_hist_", time)
                                   , value = paste0(var, "_", time)) %>%
                            mutate(activity = all_of(var)
                                   , period = all_of(time)
                                   , cohort = as_factor(cohort)
                                   , n_atleast1 = case_when(value >= 1 ~ 1
                                                            , TRUE ~ 0)
                                   , pod_ons_new = factor(pod_ons_new
                                                          , levels = c("Home", "Care home", "Hospital"
                                                                       , "Hospice", "Elsewhere/other")))) 
         , model = map(dataset, function(dset) glm(n_atleast1 ~ cohort*pod_ons_new, data = dset, family = binomial(link = "identity")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , carehome_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[3], 4))
         , carehome_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[3], 4))
         , carehome_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[3], 4))
         , hospital_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[4], 4))
         , hospital_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[4], 4))
         , hospital_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[4], 4))
         , hospice_coeff    = map_dbl(model, function(x) round(broom::tidy(x)$estimate[5], 4))
         , hospice_se       = map_dbl(model, function(x) round(broom::tidy(x)$std.error[5], 4))
         , hospice_pvalue   = map_dbl(model, function(x) round(broom::tidy(x)$p.value[5], 4))
         , other_coeff      = map_dbl(model, function(x) round(broom::tidy(x)$estimate[6], 4))
         , other_se         = map_dbl(model, function(x) round(broom::tidy(x)$std.error[6], 4))
         , other_pvalue     = map_dbl(model, function(x) round(broom::tidy(x)$p.value[6], 4))
         , interact_carehome_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[7], 4))
         , interact_carehome_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[7], 4))
         , interact_carehome_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[7], 4))
         , interact_hospital_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[8], 4))
         , interact_hospital_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[8], 4))
         , interact_hospital_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[8], 4))
         , interact_hospice_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[9], 4))
         , interact_hospice_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[9], 4))
         , interact_hospice_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[9], 4))
         , interact_other_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[10], 4))
         , interact_other_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[10], 4))
         , interact_other_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[10], 4))
         , interact_chisq_pvalue    = map_dbl(model, function(x) round(anova(x, test = "Chisq")$`Pr(>Chi)`[4], 4))
         , home_cohort_0     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , home_cohort_1     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , carehome_cohort_0 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , carehome_cohort_1 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 1, 0, 0, 0, 1, 0, 0, 0), 1)))$estimate, 3))
         , hospital_cohort_0 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , hospital_cohort_1 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 1, 0, 0, 0, 1, 0, 0), 1)))$estimate, 3))
         , hospice_cohort_0  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , hospice_cohort_1  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 1, 0, 0, 0, 1, 0), 1)))$estimate, 3))
         , other_cohort_0    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0), 1)))$estimate, 3))
         , other_cohort_1    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 1, 0, 0, 0, 1), 1)))$estimate, 3))
         , home_cohort_0_1_pvalue     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , carehome_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , hospital_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , hospice_cohort_0_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , other_cohort_0_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, -1), 1)))$adj.p.value, 4))
         , home_carehome_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, -1, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, -1, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_0_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, -1, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_0_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, 0, -1, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_carehome_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, -1, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, -1, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, -1, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, 0, -1, 0, 0, 0, -1), 1)))$adj.p.value, 4))  ) %>%
  select(-dataset, -model)

write_csv(gp_model_service_use_prop_cohort_pod, here::here("output", "describe_service_use", "complete_gp_history", "models", "gp_model_service_use_prop_cohort_pod.csv"))

# Test differences proportion with at least 1 event with binomial model and identity link

gp_model_emadm3_prop_cohort_pod <- tibble(measure = c("emadm_3m")) %>%
  mutate(activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , dataset = map2(activity, period,  function(var, time) df_input %>%
                            filter(!is.na(study_cohort)) %>% 
                            select(cohort, pod_ons_new, starts_with(var), starts_with("gp_hist")) %>%
                            select(cohort, pod_ons_new, ends_with(time)) %>%
                            rename(gp_hist = paste0("gp_hist_", time)
                                   , value = paste0(var, "_", time)) %>%
                            mutate(activity = all_of(var)
                                   , period = all_of(time)
                                   , cohort = as_factor(cohort)
                                   , n_atleast3 = case_when(value >= 3 ~ 1
                                                            , TRUE ~ 0)
                                   , pod_ons_new = factor(pod_ons_new
                                                          , levels = c("Home", "Care home", "Hospital"
                                                                       , "Hospice", "Elsewhere/other")))) 
         , model = map(dataset, function(dset) glm(n_atleast3 ~ cohort*pod_ons_new, data = dset, family = binomial(link = "identity")))
         , observations     = map_dbl(model, nobs)
         , observations     = plyr::round_any(observations, 10)
         , intercept_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[1], 4))
         , intercept_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[1], 4))
         , intercept_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[1], 4))
         , cohort_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[2], 4))
         , cohort_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[2], 4))
         , cohort_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[2], 4))
         , carehome_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[3], 4))
         , carehome_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[3], 4))
         , carehome_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[3], 4))
         , hospital_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[4], 4))
         , hospital_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[4], 4))
         , hospital_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[4], 4))
         , hospice_coeff    = map_dbl(model, function(x) round(broom::tidy(x)$estimate[5], 4))
         , hospice_se       = map_dbl(model, function(x) round(broom::tidy(x)$std.error[5], 4))
         , hospice_pvalue   = map_dbl(model, function(x) round(broom::tidy(x)$p.value[5], 4))
         , other_coeff      = map_dbl(model, function(x) round(broom::tidy(x)$estimate[6], 4))
         , other_se         = map_dbl(model, function(x) round(broom::tidy(x)$std.error[6], 4))
         , other_pvalue     = map_dbl(model, function(x) round(broom::tidy(x)$p.value[6], 4))
         , interact_carehome_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[7], 4))
         , interact_carehome_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[7], 4))
         , interact_carehome_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[7], 4))
         , interact_hospital_coeff  = map_dbl(model, function(x) round(broom::tidy(x)$estimate[8], 4))
         , interact_hospital_se     = map_dbl(model, function(x) round(broom::tidy(x)$std.error[8], 4))
         , interact_hospital_pvalue = map_dbl(model, function(x) round(broom::tidy(x)$p.value[8], 4))
         , interact_hospice_coeff   = map_dbl(model, function(x) round(broom::tidy(x)$estimate[9], 4))
         , interact_hospice_se      = map_dbl(model, function(x) round(broom::tidy(x)$std.error[9], 4))
         , interact_hospice_pvalue  = map_dbl(model, function(x) round(broom::tidy(x)$p.value[9], 4))
         , interact_other_coeff     = map_dbl(model, function(x) round(broom::tidy(x)$estimate[10], 4))
         , interact_other_se        = map_dbl(model, function(x) round(broom::tidy(x)$std.error[10], 4))
         , interact_other_pvalue    = map_dbl(model, function(x) round(broom::tidy(x)$p.value[10], 4))
         , interact_chisq_pvalue    = map_dbl(model, function(x) round(anova(x, test = "Chisq")$`Pr(>Chi)`[4], 4))
         , home_cohort_0     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , home_cohort_1     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , carehome_cohort_0 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 1, 0, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , carehome_cohort_1 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 1, 0, 0, 0, 1, 0, 0, 0), 1)))$estimate, 3))
         , hospital_cohort_0 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , hospital_cohort_1 = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 1, 0, 0, 0, 1, 0, 0), 1)))$estimate, 3))
         , hospice_cohort_0  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0), 1)))$estimate, 3))
         , hospice_cohort_1  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 1, 0, 0, 0, 1, 0), 1)))$estimate, 3))
         , other_cohort_0    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0), 1)))$estimate, 3))
         , other_cohort_1    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(1, 1, 0, 0, 0, 1, 0, 0, 0, 1), 1)))$estimate, 3))
         , home_cohort_0_1_pvalue     = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , carehome_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , hospital_cohort_0_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , hospice_cohort_0_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , other_cohort_0_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, -1, 0, 0, 0, 0, 0, 0, 0, -1), 1)))$adj.p.value, 4))
         , home_carehome_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, -1, 0, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_0_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, -1, 0, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_0_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, -1, 0, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_0_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 0, 0, 0, 0, -1, 0, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_carehome_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, -1, 0, 0, 0, -1, 0, 0, 0), 1)))$adj.p.value, 4))
         , home_hospital_cohort_1_pvalue = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, -1, 0, 0, 0, -1, 0, 0), 1)))$adj.p.value, 4))
         , home_hospice_cohort_1_pvalue  = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, -1, 0, 0, 0, -1, 0), 1)))$adj.p.value, 4))
         , home_other_cohort_1_pvalue    = map_dbl(model, function(x) round(broom::tidy(multcomp::glht(x, linfct = matrix(c(0, 1, 0, 0, 0, -1, 0, 0, 0, -1), 1)))$adj.p.value, 4))  ) %>%
  select(-dataset, -model)

write_csv(gp_model_emadm3_prop_cohort_pod, here::here("output", "describe_service_use", "complete_gp_history", "models", "gp_model_emadm3_prop_cohort_pod.csv"))

################################################################################
