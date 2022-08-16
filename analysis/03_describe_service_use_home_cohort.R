################################################################################

########## DESCRIBE SERVICE USE HOME COHORT ##########

################################################################################

# Describe service use for home deaths - focus on inequalities

# Service use for home deaths by cohort and characteristic
# Plots of service use by cohort and characteristic
# Significance test mean difference between cohorts for each characteristic

################################################################################

########## Libraries ##########

library("tidyverse")
library("lubridate")

################################################################################

########## Save location ##########

fs::dir_create(here::here("output", "describe_service_use_home"))
fs::dir_create(here::here("output", "describe_service_use_home", "plots"))

################################################################################

########## NT chart style ##########

# Nuffield Trust colour list

NT_colours <- c(
  `NT ink` = "#271544",
  `white` = "#FFFFFF",
  `NT iris` = "#AC8ACF",
  `cool black` = "#0E1B26",
  `cool dark grey` = "#556370",
  `cool mid grey` = "#9AA0AA",
  `cool light grey` = "#F4F4F4",
  `bright purple` = "#9F67FF",
  `light purple 1` = "#D3C4FC",
  `light purple 2` = "#B39DFF",
  `dark purple 1` = "#7140EA",
  `dark purple 2` = "#49148C",
  `bright blue` = "#0066F4",
  `light blue 1` = "#99DBFF",
  `light blue 2` = "#63B2FF",
  `dark blue 1` = "#005AC7",
  `dark blue 2` = "#192889",
  `bright red` = "#FF6B57",
  `light red 1` = "#FFCFC9",
  `light red 2` = "#FF997F",
  `dark red 1` = "#B71C1C",
  `dark red 2` = "#700C28",
  `bright yellow` = "#EABE17",
  `light yellow 1` = "#FDEA9D",
  `light yellow 2` = "#F4D05A",
  `dark yellow 1` = "#DD931C",
  `dark yellow 2` = "#B26605",
  `bright green` = "#00C27A",
  `light green 1` = "#8BF8BD",
  `light green 2` = "#39DA91",
  `dark green 1` = "#00823F",
  `dark green 2` = "#195442",
  `bright cyan` = "#4DCFF5",
  `light cyan 1` = "#9EF7FF",
  `light cyan 2` = "#6AE8F9",
  `dark cyan 1` = "#008CB3",
  `dark cyan 2` = "#004C70"
)

NT_colour <- function(index = NULL, named = FALSE){
  
  if(is.null(index)){
    index <- names(NT_colours)
  }
  
  return_value <- NT_colours[index]
  if (!named) {
    names(return_value) <- NULL
  }
  
  return(return_value)
  
}

####################################

# NT colour palette

NT_palette <- function(NT_theme = NULL, reverse = FALSE, ...) {
  
  function(n) {
    
    stopifnot(n <= 5 | (n <= 12 & (is.null(NT_theme) | NT_theme == "bright")))
    
    colour_indices <-
      if (n == 1 & is.null(NT_theme)) { "bright purple" }
    else if (n == 2 & is.null(NT_theme)) { c("bright purple", "bright green") }
    else if (n == 3 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue") }
    else if (n == 4 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow") }
    else if (n == 5 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red") }
    else if (n == 6 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan") }
    else if (n == 7 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1") }
    else if (n == 8 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1") }
    else if (n == 9 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1") }
    else if (n == 10 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1") }
    else if (n == 11 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1", "light red 1") }
    else if (n == 12 & is.null(NT_theme)) { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1", "light red 1", "light cyan 1") }
    else if (n == 1 & NT_theme == "bright") { "bright purple" }
    else if (n == 2 & NT_theme == "bright") { c("bright purple", "bright green") }
    else if (n == 3 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue") }
    else if (n == 4 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow") }
    else if (n == 5 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red") }
    else if (n == 6 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan") }
    else if (n == 7 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1") }
    else if (n == 8 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1") }
    else if (n == 9 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1") }
    else if (n == 10 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1") }
    else if (n == 11 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1", "light red 1") }
    else if (n == 12 & NT_theme == "bright") { c("bright purple", "bright green", "bright blue", "bright yellow", "bright red", "bright cyan", "light purple 1", "light green 1", "light blue 1", "light yellow 1", "light red 1", "light cyan 1") }
    else if (n == 1 & NT_theme == "purple") { "bright purple" }
    else if (n == 2 & NT_theme == "purple") { c("dark purple 2", "bright purple") }
    else if (n == 3 & NT_theme == "purple") { c("dark purple 2", "bright purple", "light purple 2") }
    else if (n == 4 & NT_theme == "purple") { c("dark purple 2", "dark purple 1", "bright purple", "light purple 2") }
    else if (n == 5 & NT_theme == "purple") { c("dark purple 2", "dark purple 1", "bright purple", "light purple 2", "light purple 1") }
    else if (n == 1 & NT_theme == "blue") { "bright blue" }
    else if (n == 2 & NT_theme == "blue") { c("dark blue 2", "bright blue") }
    else if (n == 3 & NT_theme == "blue") { c("dark blue 2", "bright blue", "light blue 2") }
    else if (n == 4 & NT_theme == "blue") { c("dark blue 2", "dark blue 1", "bright blue", "light blue 2") }
    else if (n == 5 & NT_theme == "blue") { c("dark blue 2", "dark blue 1", "bright blue", "light blue 2", "light blue 1") }
    else if (n == 1 & NT_theme == "red") { "bright red" }
    else if (n == 2 & NT_theme == "red") { c("dark red 2", "bright red") }
    else if (n == 3 & NT_theme == "red") { c("dark red 2", "bright red", "light red 2") }
    else if (n == 4 & NT_theme == "red") { c("dark red 2", "dark red 1", "bright red", "light red 2") }
    else if (n == 5 & NT_theme == "red") { c("dark red 2", "dark red 1", "bright red", "light red 2", "light red 1") }
    else if (n == 1 & NT_theme == "yellow") { "bright yellow" }
    else if (n == 2 & NT_theme == "yellow") { c("dark yellow 2", "bright yellow") }
    else if (n == 3 & NT_theme == "yellow") { c("dark yellow 2", "bright yellow", "light yellow 2") }
    else if (n == 4 & NT_theme == "yellow") { c("dark yellow 2", "dark yellow 1", "bright yellow", "light yellow 2") }
    else if (n == 5 & NT_theme == "yellow") { c("dark yellow 2", "dark yellow 1", "bright yellow", "light yellow 2", "light yellow 1") }
    else if (n == 1 & NT_theme == "green") { "bright green" }
    else if (n == 2 & NT_theme == "green") { c("dark green 2", "bright green") }
    else if (n == 3 & NT_theme == "green") { c("dark green 2", "bright green", "light green 2") }
    else if (n == 4 & NT_theme == "green") { c("dark green 2", "dark green 1", "bright green", "light green 2") }
    else if (n == 5 & NT_theme == "green") { c("dark green 2", "dark green 1", "bright green", "light green 2", "light green 1") }
    else if (n == 1 & NT_theme == "cyan") { "bright cyan" }
    else if (n == 2 & NT_theme == "cyan") { c("dark cyan 2", "bright cyan") }
    else if (n == 3 & NT_theme == "cyan") { c("dark cyan 2", "bright cyan", "light cyan 2") }
    else if (n == 4 & NT_theme == "cyan") { c("dark cyan 2", "dark cyan 1", "bright cyan", "light cyan 2") }
    else if (n == 5 & NT_theme == "cyan") { c("dark cyan 2", "dark cyan 1", "bright cyan", "light cyan 2", "light cyan 1") }
    
    return_colours <- NT_colour(colour_indices)
    
    if (reverse) {
      
      return_colours <- rev(NT_colour(colour_indices))
      
    }
    
    return(return_colours)
    
  }
}

####################################

# NT colour scale

scale_colour_NT <- function(palette = NT_palette(NT_theme = NULL, reverse = FALSE, ...), ...) {
  
  ggplot2::discrete_scale(
    aesthetics = "colour",
    scale_name = "NT1",
    palette = palette,
    na.value = "#9AA0AA",
    ...
  )
  
}

####################################

# NT fill scale

scale_fill_NT <- function(palette = NT_palette(NT_theme = NULL, reverse = FALSE, ...), ...) {
  
  ggplot2::discrete_scale(
    aesthetics = "fill",
    scale_name = "NT2",
    palette = palette,
    na.value = "#9AA0AA",
    ...
  )
  
}

####################################

# NT ggplot theme

NT_style <- function(){
  
  font <- "TT Arial"
  family <- "sans"
  
  theme_minimal() %+replace%
    theme(
      # Background elements
      panel.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
      panel.border = element_blank(),
      plot.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit ="cm"),
      # Grid elements
      axis.ticks = element_blank(),      
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#9AA0AA", size = 0.3),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(0.5, "cm"),
      # Text elements
      axis.text.x = element_text(colour = "#9AA0AA", size = 8, family = "sans", vjust = 0),
      axis.text.y = element_text(colour = "#9AA0AA", size = 8, family = "sans"),
      axis.title.x = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit ="cm"), colour = "#271544", size = 8, face = "bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 0.3, b = 0, l = 0, unit ="cm"), colour = "#271544", size = 8, face = "bold", angle = 90),
      legend.text = element_text(colour = "#271544", size = 8, face = "bold", family = "sans"),
      legend.title = element_blank(),
      plot.caption = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit ="cm"), colour = "#271544", size = 8, hjust = 1, vjust = 1),
      plot.title = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit ="cm"), colour = "#271544", size = 10, face = "bold", hjust = 0),
      plot.title.position = "plot",
      strip.text = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit ="cm"), colour = "#271544", size = 8, face = "bold"),
      # Legend elements
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit ="cm"),
      legend.key = element_blank(),
      legend.key.size = unit(0.4, "cm"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit ="cm"),
      legend.position = "bottom",
      legend.spacing.x = unit(0.1, "cm"),
      legend.spacing.y = unit(0.1, "cm")
    )
}

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
         , agegrp = case_when(age >= 0 & age <= 29 ~ "00-29"
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

########## Service use of home deaths by cohort and characteristics ##########

characteristics <- c("sex", "agegrp", "ethnicity", "ltcgrp", "palcare", "nopalcare", "carehome", "region", "imd_quintile", "imd_quintile_la"
                     , "rural_urban", "codgrp")

service_use_cohort_home <- tidyr::expand_grid(measure = unique(df_input %>%
                                                                 select(ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
                                                                 select(-contains("gp_hist")) %>% 
                                                                 pivot_longer(cols = everything(), names_to = "measure", values_to = "value") %>%
                                                                 select(measure))$measure
                                              , char_category = unique(df_input %>%
                                                                         select(all_of(characteristics)) %>%
                                                                         mutate(across(everything(), as_factor)) %>%
                                                                         pivot_longer(cols = everything(), names_to = "characteristic", values_to = "category") %>%
                                                                         mutate(char_category = paste0(characteristic, "_", category)) %>%
                                                                         select(char_category))$char_category) %>%
  mutate(dataset_0 = map2(measure, char_category, function(var, char) df_input %>%
                            filter(!is.na(study_cohort) & pod_ons_new == "Home" & cohort == 0) %>% 
                            select(all_of(characteristics), all_of(var)) %>%
                            mutate(across(all_of(characteristics), as_factor)) %>%
                            pivot_longer(cols = -c(all_of(var)), names_to = "characteristic", values_to = "category") %>%
                            mutate(char_category = paste0(characteristic, "_", category)
                                   , measure = all_of(var)) %>%
                            rename(value = all_of(var)) %>%
                            filter(char_category == char))
         , dataset_1 = map2(measure, char_category, function(var, char) df_input %>%
                              filter(!is.na(study_cohort) & pod_ons_new == "Home" & cohort == 1) %>% 
                              select(all_of(characteristics), all_of(var)) %>%
                              mutate(across(all_of(characteristics), as_factor)) %>%
                              pivot_longer(cols = -c(all_of(var)), names_to = "characteristic", values_to = "category") %>%
                              mutate(char_category = paste0(characteristic, "_", category)
                                     , measure = all_of(var)) %>%
                              rename(value = all_of(var)) %>%
                              filter(char_category == char))
         , activity = str_sub(measure, 1, -4)
         , period = str_sub(measure, -2, -1)
         , n_cohort_0 = map_dbl(dataset_0, function(dset0) plyr::round_any(nrow(dset0), 10))
         , n_cohort_1 = map_dbl(dataset_1, function(dset1) plyr::round_any(nrow(dset1), 10))
         , mean_cohort_0 = map_dbl(dataset_0, function(dset0) round(mean(dset0$value, na.rm = TRUE), 3))
         , mean_cohort_1 = map_dbl(dataset_1, function(dset1) round(mean(dset1$value, na.rm = TRUE), 3))
         , sd_cohort_0 = map_dbl(dataset_0, function(dset0) round(sd(dset0$value, na.rm = TRUE), 3))
         , sd_cohort_1 = map_dbl(dataset_1, function(dset1) round(sd(dset1$value, na.rm = TRUE), 3))
         , n_atleast1_cohort_0 = map_dbl(dataset_0, function(dset0) plyr::round_any(sum(dset0$value >= 1, na.rm = TRUE), 10))
         , n_atleast1_cohort_1 = map_dbl(dataset_1, function(dset1) plyr::round_any(sum(dset1$value >= 1, na.rm = TRUE), 10)) 
         , n_atleast3_cohort_0 = map_dbl(dataset_0, function(dset0) plyr::round_any(sum(dset0$value >= 3, na.rm = TRUE), 10))
         , n_atleast3_cohort_1 = map_dbl(dataset_1, function(dset1) plyr::round_any(sum(dset1$value >= 3, na.rm = TRUE), 10)) 
         , mean_cohort_0 = case_when(n_atleast1_cohort_0 == 0 ~ 0
                                     , TRUE ~ round(mean_cohort_0, 3))
         , mean_cohort_1 = case_when(n_atleast1_cohort_1 == 0 ~ 0
                                     , TRUE ~ round(mean_cohort_1, 3))
         , sd_cohort_0 = case_when(n_atleast1_cohort_0 == 0 ~ 0
                                   , TRUE ~ round(sd_cohort_0, 3))
         , sd_cohort_1 = case_when(n_atleast1_cohort_1 == 0 ~ 0
                                   , TRUE ~ round(sd_cohort_1, 3))
         , characteristic = str_sub(str_extract(char_category, "^[:graph:]+[_a-z$]"), 1, nchar(str_extract(char_category, "^[:graph:]+[_a-z$]"))-1)
         , category = str_extract(char_category, "[^_]+$")) %>%
  arrange(category, characteristic, activity, period)

########## Significance test cohorts mean difference ##########

# At least one of the cohorts has to have values for these to work

service_use_cohort_home_sigtest <- service_use_cohort_home %>%
  filter(n_atleast1_cohort_0 > 0 | n_atleast1_cohort_1 > 0) %>%
  mutate(normality_pvalue_0 = map_dbl(dataset_0, function(dset0) round(ks.test(dset0$value, "pnorm")$p.value, 4))
         , normality_pvalue_1 = map_dbl(dataset_1, function(dset1) round(ks.test(dset1$value, "pnorm")$p.value, 4))
         , equal_variance_pvalue = map2_dbl(dataset_0, dataset_1, function(dset0, dset1) round(var.test(dset0$value, dset1$value)$p.value, 4))
         , ttest_pvalue = map2_dbl(dataset_0, dataset_1, function(dset0, dset1) round(t.test(dset0$value, dset1$value, var.equal = TRUE)$p.value, 4))
         , welch_pvalue = map2_dbl(dataset_0, dataset_1, function(dset0, dset1) round(t.test(dset0$value, dset1$value, var.equal = FALSE)$p.value, 4))
         , wilcox_pvalue = map2_dbl(dataset_0, dataset_1, function(dset0, dset1) round(wilcox.test(dset0$value, dset1$value, exact = FALSE)$p.value, 4))
         , characteristic = str_sub(str_extract(char_category, "^[:graph:]+[_a-z$]"), 1, nchar(str_extract(char_category, "^[:graph:]+[_a-z$]"))-1)
         , category = str_extract(char_category, "[^_]+$")) %>%
  arrange(category, characteristic, activity, period) %>%
  select(-dataset_0, -dataset_1) 

write_csv(service_use_cohort_home %>% 
            select(-dataset_0, -dataset_1) %>%
            left_join(service_use_cohort_home_sigtest %>%
                        select(category, characteristic, activity, period, 
                               normality_pvalue_0, normality_pvalue_1, equal_variance_pvalue, 
                               ttest_pvalue, welch_pvalue, wilcox_pvalue)
                      , by = c("category", "characteristic", "activity", "period"))
          , here::here("output", "describe_service_use_home", "service_use_cohort_home.csv"))

################################################################################

########## Plot service use by cohort and characteristic ##########

activity <- unique(df_input %>%
                     select(ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
                     select(-contains("gp_hist")) %>% 
                     pivot_longer(cols = everything(), names_to = "measure", values_to = "value") %>%
                     mutate(activity = str_sub(measure, 1, -4)) %>% 
                     select(activity))$activity

plots_service_use_cohort_char <- tidyr::expand_grid(characteristics, activity) %>%
  mutate(dataset = map2(characteristics, activity, function(var, service) df_input %>%
                          filter(!is.na(study_cohort) & pod_ons_new == "Home") %>% 
                          rename(variable = var) %>% 
                          select(cohort, variable, starts_with(service)) %>%
                          select(-contains("gp_hist")) %>% 
                          pivot_longer(cols = -c(cohort, variable), names_to = "measure", values_to = "value") %>%
                          group_by(cohort, variable, measure) %>%
                          summarise(n = n()
                                    , mean = mean(value, na.rm = TRUE)
                                    , sd = sd(value, na.rm = TRUE)
                                    , n_atleast1 = sum(value >= 1, na.rm = TRUE)) %>%
                          mutate(n = plyr::round_any(n, 10)
                                 , n_atleast1 = plyr::round_any(n_atleast1, 10)
                                 , mean = case_when(n_atleast1 == 0 ~ 0
                                                    , TRUE ~ round(mean, 3))
                                 , sd = case_when(n_atleast1 == 0 ~ 0
                                                  , TRUE ~ round(sd, 3))) %>% 
                          mutate(activity = str_sub(measure, 1, -4)
                                 , period = str_sub(measure, -2, -1)))
         , plot_1m = map2(characteristics, dataset, function(var, dset) ggplot(dset %>% filter(period == "1m")
                                                                               , aes(x = factor(variable), y = mean
                                                                                     , colour = factor(cohort), fill = factor(cohort))) +
                            geom_bar(stat = "identity", position = "dodge") +
                            labs(x = "Category", y = "Events per person"
                                 , title = var) +
                            guides(colour = guide_legend(byrow = TRUE)) +
                            scale_colour_NT(palette = NT_palette()) +
                            scale_fill_NT(palette = NT_palette()) +
                            scale_x_discrete() +
                            scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
                            NT_style())
         , plot_3m = map2(characteristics, dataset, function(var, dset) ggplot(dset %>% filter(period == "3m")
                                                                               , aes(x = factor(variable), y = mean
                                                                                     , colour = factor(cohort), fill = factor(cohort))) +
                            geom_bar(stat = "identity", position = "dodge") +
                            labs(x = "Category", y = "Events per person"
                                 , title = var) +
                            guides(colour = guide_legend(byrow = TRUE)) +
                            scale_colour_NT(palette = NT_palette()) +
                            scale_fill_NT(palette = NT_palette()) +
                            scale_x_discrete() +
                            scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
                            NT_style())
         , plot_1y = map2(characteristics, dataset, function(var, dset) ggplot(dset %>% filter(period == "1y")
                                                                               , aes(x = factor(variable), y = mean
                                                                                     , colour = factor(cohort), fill = factor(cohort))) +
                            geom_bar(stat = "identity", position = "dodge") +
                            labs(x = "Category", y = "Events per person"
                                 , title = var) +
                            guides(colour = guide_legend(byrow = TRUE)) +
                            scale_colour_NT(palette = NT_palette()) +
                            scale_fill_NT(palette = NT_palette()) +
                            scale_x_discrete() +
                            scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
                            NT_style())
         , plot_prop_1m = map2(characteristics, dataset, function(var, dset) ggplot(dset %>% filter(period == "1m")
                                                                                    , aes(x = factor(variable), y = n_atleast1/n
                                                                                          , colour = factor(cohort), fill = factor(cohort))) +
                                 geom_bar(stat = "identity", position = "dodge") +
                                 labs(x = "Category", y = "Proportion with event"
                                      , title = var) +
                                 guides(colour = guide_legend(byrow = TRUE)) +
                                 scale_colour_NT(palette = NT_palette()) +
                                 scale_fill_NT(palette = NT_palette()) +
                                 scale_x_discrete() +
                                 scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
                                 NT_style())
         , plot_prop_3m = map2(characteristics, dataset, function(var, dset) ggplot(dset %>% filter(period == "3m")
                                                                                    , aes(x = factor(variable), y = n_atleast1/n
                                                                                          , colour = factor(cohort), fill = factor(cohort))) +
                                 geom_bar(stat = "identity", position = "dodge") +
                                 labs(x = "Category", y = "Proportion with event"
                                      , title = var) +
                                 guides(colour = guide_legend(byrow = TRUE)) +
                                 scale_colour_NT(palette = NT_palette()) +
                                 scale_fill_NT(palette = NT_palette()) +
                                 scale_x_discrete() +
                                 scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
                                 NT_style())
         , plot_prop_1y = map2(characteristics, dataset, function(var, dset) ggplot(dset %>% filter(period == "1y")
                                                                                    , aes(x = factor(variable), y = n_atleast1/n
                                                                                          , colour = factor(cohort), fill = factor(cohort))) +
                                 geom_bar(stat = "identity", position = "dodge") +
                                 labs(x = "Category", y = "Proportion with event"
                                      , title = var) +
                                 guides(colour = guide_legend(byrow = TRUE)) +
                                 scale_colour_NT(palette = NT_palette()) +
                                 scale_fill_NT(palette = NT_palette()) +
                                 scale_x_discrete() +
                                 scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
                                 NT_style())
  )

save_cohort_plots <- function(service) { 
  
  filename_1m <- paste0("output/describe_service_use_home/plots/service_use_cohort_home_", service, "_1m.pdf")
  
  pdf(filename_1m)
  
  print((plots_service_use_cohort_char %>% filter(activity == service))$plot_1m)  
  
  dev.off()
  
  
  filename_3m <- paste0("output/describe_service_use_home/plots/service_use_cohort_home_", service, "_3m.pdf")
  
  pdf(filename_3m)
  
  print((plots_service_use_cohort_char %>% filter(activity == service))$plot_3m)  
  
  dev.off()
  
  
  filename_1y <- paste0("output/describe_service_use_home/plots/service_use_cohort_home_", service, "_1y.pdf")
  
  pdf(filename_1y)
  
  print((plots_service_use_cohort_char %>% filter(activity == service))$plot_1y)  
  
  dev.off()
  
  
  filename_prop_1m <- paste0("output/describe_service_use_home/plots/service_prop_cohort_home_", service, "_1m.pdf")
  
  pdf(filename_prop_1m)
  
  print((plots_service_use_cohort_char %>% filter(activity == service))$plot_prop_1m)  
  
  dev.off()
  
  
  filename_prop_3m <- paste0("output/describe_service_use_home/plots/service_prop_cohort_home_", service, "_3m.pdf")
  
  pdf(filename_prop_3m)
  
  print((plots_service_use_cohort_char %>% filter(activity == service))$plot_prop_3m)  
  
  dev.off()
  
  
  filename_prop_1y <- paste0("output/describe_service_use_home/plots/service_prop_cohort_home_", service, "_1y.pdf")
  
  pdf(filename_prop_1y)
  
  print((plots_service_use_cohort_char %>% filter(activity == service))$plot_prop_1y)  
  
  dev.off()
  
}

lapply(activity, save_cohort_plots)

################################################################################
