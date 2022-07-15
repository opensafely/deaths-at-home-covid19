################################################################################

########## DESCRIBE COHORTS ##########

################################################################################

# Number of deaths by cohort and quarter
# Number of deaths by place - cohort and quarter
# Compare deaths by place, quarter and cause to inform cause of death groups
# Assess completeness of cohorts relative to deaths reported by ONS
# Number of deaths by cohort and characteristic with ratio comparison
# Summary table cohort characteristics
# Number of deaths by quarter and characteristics
# Summary table quarter characteristics
# Number of deaths by cohort, place of death and characteristics
# Summary table death characteristics by place and cohort
# Number of deaths by quarter, place of death and characteristics
# Summary table death characteristics by place and quarter

################################################################################

########## Libraries ##########

library("tidyverse")
library("lubridate")

################################################################################

########## Save locations ##########

fs::dir_create(here::here("output", "describe_cohorts"))
fs::dir_create(here::here("output", "describe_cohorts", "overall_death_counts"))
fs::dir_create(here::here("output", "describe_cohorts", "quarter_death_counts"))
fs::dir_create(here::here("output", "describe_cohorts", "ons_death_comparisons"))
fs::dir_create(here::here("output", "describe_cohorts", "death_ratios_cohort"))
fs::dir_create(here::here("output", "describe_cohorts", "death_ratios_pod_cohort"))

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

########## Basic death counts ##########

# Number of deaths in each year of study

deaths_data <- df_input %>%
  group_by(cohort) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10))

write_csv(deaths_data, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_data.csv"))

# Number of deaths by study cohort

deaths_cohort <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10))

write_csv(deaths_cohort, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_cohort.csv"))

# Number of deaths by quarter
deaths_quarter <- df_input %>%
  group_by(study_quarter) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10))

write_csv(deaths_quarter, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_quarter.csv"))

################################################################################

########## Plot basic death counts ##########

# Deaths by quarter

plot_deaths_quarter <- ggplot(deaths_quarter) + 
  geom_bar(aes(x = study_quarter, y = deaths), stat = "identity", fill = "#9F67FF") +
  labs(x = "Study quarter", y = "Number of deaths") +
  scale_x_continuous(breaks = seq(1, 8, 1)) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  NT_style()

ggsave(plot = plot_deaths_quarter, filename ="deaths_quarter.png", path = here::here("output", "describe_cohorts", "overall_death_counts"), height = 10, width = 13.7, units = "cm", dpi = 600)

################################################################################

########## Death counts by place ##########

# Number of deaths in each year of study by place of death

deaths_data_pod <- df_input %>%
  group_by(cohort, pod_ons_new) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total)

write_csv(deaths_data_pod, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_data_pod.csv"))

# Number of deaths by study cohort and place of death

deaths_cohort_pod <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total)

write_csv(deaths_cohort_pod, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_cohort_pod.csv"))

# Number of deaths by quarter and place of death

deaths_quarter_pod1 <- df_input %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total)

write_csv(deaths_quarter_pod1, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_quarter_pod.csv"))

################################################################################

########## Plot death counts by place ##########

# Plot of number of deaths by place and cohort

plot_deaths_pod_cohort <- ggplot(deaths_cohort_pod) + 
  geom_bar(aes(x = reorder(pod_ons_new, deaths), y = deaths, fill = factor(cohort, levels = c("1", "0"))), stat = "identity", position = "dodge", width = 0.6) +
  coord_flip() +
  labs(x = "Place of death", y = "Number of deaths") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_manual(values = c("0" = "#00C27A", "1" = "#9F67FF"), labels = c("0" = "Pre-pandemic", "1" = "Pandemic"), breaks = c("0", "1")) +
  NT_style() +
  theme(
    axis.text.y = element_text(colour = "#9AA0AA", size = 8, family = "sans", hjust = 1),
    panel.grid.major.x = element_line(colour = "#9AA0AA", size = 0.3),
    panel.grid.major.y = element_blank())

ggsave(plot = plot_deaths_pod_cohort, filename ="deaths_pod_cohort.png", path = here::here("output", "describe_cohorts", "overall_death_counts"), height = 10, width = 13.7, units = "cm", dpi = 600)

# Plot of proportion of deaths by place and cohort

plot_deaths_pod_cohort_prop <- ggplot(deaths_cohort_pod) + 
  geom_bar(aes(x = reorder(pod_ons_new, proportion), y = proportion, fill = factor(cohort, levels = c("1", "0"))), stat = "identity", position = "dodge", width = 0.6) +
  coord_flip() +
  labs(x = "Place of death", y = "Proportion of deaths") +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_manual(values = c("0" = "#00C27A", "1" = "#9F67FF"), labels = c("0" = "Pre-pandemic", "1" = "Pandemic"), breaks = c("0", "1")) +
  NT_style() +
  theme(
    axis.text.y = element_text(colour = "#9AA0AA", size = 8, family = "sans", hjust = 1),
    panel.grid.major.x = element_line(colour = "#9AA0AA", size = 0.3),
    panel.grid.major.y = element_blank())

ggsave(plot = plot_deaths_pod_cohort_prop, filename ="deaths_pod_cohort_prop.png", path = here::here("output", "describe_cohorts", "overall_death_counts"), height = 10, width = 13.7, units = "cm", dpi = 600)

################################################################################

########## Death counts by place, cohort and cause of death ##########

# Number of deaths by cohort, place of death and cause of death
# Help to decide which cause of death groupings to use

deaths_cohort_pod_lcod <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  mutate(lcod_ons_grp = case_when(cod_ons_3 >= "A00" & cod_ons_3 <= "A09" ~ "Intestinal infectious diseases"
                                 , (cod_ons_3 >= "A15" & cod_ons_3 <= "A19") | cod_ons_3 == "B90" ~ "Tuberculosis"
                                 , cod_ons_3 %in% c("A20", "A44") | (cod_ons_3 >= "A75" & cod_ons_3 <= "A79") | (cod_ons_3 >= "A82" & cod_ons_3 <= "A84") | cod_ons_4 == "A852" | (cod_ons_3 >= "A90" & cod_ons_3 <= "A98") | (cod_ons_3 >= "B50" & cod_ons_3 <= "B57") ~ "Vector-borne diseases and rabies"
                                 , (cod_ons_3 >= "A33" & cod_ons_3 <= "A37") | cod_ons_4 == "A492" | cod_ons_3 %in% c("A80", "B01", "B02", "B05", "B06", "B15", "B16") | cod_ons_4 %in% c("B170", "B180", "B181") | cod_ons_3 %in% c("B26", "B91", "G14") ~ "Vaccine-preventable diseases"
                                 , cod_ons_3 %in% c("A39", "A87") | (cod_ons_3 >= "G00" & cod_ons_3 <= "G03") ~ "Meningitis and meningococcal infection"
                                 , cod_ons_3 >= "A40" & cod_ons_3 <= "A41" ~ "Septicaemia"
                                 , cod_ons_3 >= "B20" & cod_ons_3 <= "B24" ~ "HIV"
                                 , cod_ons_3 == "C15" ~ "Malignant neoplasm of oesophagus"
                                 , cod_ons_3 == "C16" ~ "Malignant neoplasm of stomach"
                                 , cod_ons_3 >= "C18" & cod_ons_3 <= "C21" ~ "Malignant neoplasm of colon, sigmoid, rectum and anus"
                                 , cod_ons_3 == "C22" ~ "Malignant neoplasm of liver and intrahepatic bile ducts"
                                 , cod_ons_3 >= "C23" & cod_ons_3 <= "C24" ~ "Malignant neoplasm of gallbladder and other parts of biliary tract"
                                 , cod_ons_3 == "C25" ~ "Malignant neoplasm of pancreas"
                                 , cod_ons_3 == "C32" ~ "Malignant neoplasm of larynx"
                                 , cod_ons_3 >= "C33" & cod_ons_3 <= "C34" ~ "Malignant neoplasm of trachea, bronchus and lung"
                                 , cod_ons_3 >= "C40" & cod_ons_3 <= "C41" ~ "Malignant neoplasm of bone and articular cartilage"
                                 , cod_ons_3 >= "C43" & cod_ons_3 <= "C44" ~ "Melanoma and other malignant neoplasms of skin"
                                 , cod_ons_3 == "C50" ~ "Malignant neoplasm of breast"
                                 , cod_ons_3 >= "C53" & cod_ons_3 <= "C55" ~ "Malignant neoplasm of uterus"
                                 , cod_ons_3 == "C56" ~ "Malignant neoplasm of ovary"
                                 , cod_ons_3 == "C61" ~ "Malignant neoplasm of prostate"
                                 , cod_ons_3 == "C64" ~ "Malignant neoplasm of kidney, except renal pelvis"
                                 , cod_ons_3 == "C67" ~ "Malignant neoplasm of bladder"
                                 , cod_ons_3 == "C71" ~ "Malignant neoplasm of brain"
                                 , cod_ons_3 >= "C81" & cod_ons_3 <= "C96" ~ "Malignant neoplasms, stated or presumed to be primary of lymphoid, haematopoietic and related tissue"
                                 , cod_ons_3 >= "D00" & cod_ons_3 <= "D48" ~ "In situ and benign neoplasms, and neoplasms of uncertain or unknown behaviour"
                                 , cod_ons_3 >= "E10" & cod_ons_3 <= "E14" ~ "Diabetes"
                                 , (cod_ons_3 >= "D50" & cod_ons_3 <= "D53") | (cod_ons_3 >= "E40" & cod_ons_3 <= "E64") ~ "Malnutrition, nutritional anaemias and other nutritional deficiencies"
                                 , cod_ons_3 >= "E86" & cod_ons_3 <= "E87" ~ "Disorders of fluid, electrolyte and acid-base balance"
                                 , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer disease"
                                 , cod_ons_3 >= "F10" & cod_ons_3 <= "F19" ~ "Mental and behavioural disorders due to psychoactive substance use"
                                 , cod_ons_3 >= "G10" & cod_ons_3 <= "G12" ~ "Systemic atrophies primarily affecting the central nervous system"
                                 , cod_ons_3 == "G20" ~ "Parkinson disease"
                                 , cod_ons_3 >= "G40" & cod_ons_3 <= "G41" ~ "Epilepsy and status epilepticus"
                                 , cod_ons_3 >= "G80" & cod_ons_3 <= "G83" ~ "Cerebral palsy and other paralytic syndromes"
                                 , cod_ons_3 >= "I05" & cod_ons_3 <= "I09" ~ "Chronic rheumatic heart diseases"
                                 , cod_ons_3 >= "I10" & cod_ons_3 <= "I15" ~ "Hypertensive diseases"
                                 , cod_ons_3 >= "I20" & cod_ons_3 <= "I25" ~ "Ischaemic heart diseases"
                                 , cod_ons_3 >= "I26" & cod_ons_3 <= "I28" ~ "Pulmonary heart disease and diseases of pulmonary circulation"
                                 , cod_ons_3 >= "I34" & cod_ons_3 <= "I38" ~ "Nonrheumatic valve disorders and endocarditis"
                                 , cod_ons_3 == "I42" ~ "Cardiomyopathy"
                                 , cod_ons_3 == "I46" ~ "Cardiac arrest"
                                 , cod_ons_3 >= "I47" & cod_ons_3 <= "I49" ~ "Cardiac arrhythmias"
                                 , cod_ons_3 >= "I50" & cod_ons_3 <= "I51" ~ "Heart failure and complications and ill-defined heart disease"
                                 , cod_ons_3 >= "I60" & cod_ons_3 <= "I69" ~ "Cerebrovascular diseases"
                                 , cod_ons_3 == "I70" ~ "Atherosclerosis"
                                 , cod_ons_3 == "I71" ~ "Aortic aneurysm and dissection"
                                 , (cod_ons_3 >= "J00" & cod_ons_3 <= "J06") | (cod_ons_3 >= "J20" & cod_ons_3 <= "J22") ~ "Acute respiratory infections other than influenza and pneumonia"
                                 , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Influenza and pneumonia"
                                 , cod_ons_3 >= "J40" & cod_ons_3 <= "J47" ~ "Chronic lower respiratory diseases"
                                 , cod_ons_3 >= "J80" & cod_ons_3 <= "J84" ~ "Pulmonary oedema and other interstitial pulmonary diseases"
                                 , cod_ons_3 == "J96" ~ "Respiratory failure"
                                 , (cod_ons_3 >= "K35" & cod_ons_3 <= "K46") | cod_ons_3 == "K56" ~ "Appendicitis, hernia and intestinal obstruction"
                                 , cod_ons_3 >= "K70" & cod_ons_3 <= "K76" ~ "Cirrhosis and other diseases of liver"
                                 , cod_ons_3 >= "M00" & cod_ons_3 <= "M99" ~ "Diseases of musculoskeletal system and connective tissue"
                                 , cod_ons_3 >= "N00" & cod_ons_3 <= "N39" ~ "Diseases of the urinary system"
                                 , cod_ons_3 >= "O00" & cod_ons_3 <= "O99" ~ "Pregnancy, childbirth and puerperium"
                                 , cod_ons_3 >= "P00" & cod_ons_3 <= "P96" ~ "Certain conditions originating in the perinatal period"
                                 , cod_ons_3 >= "Q00" & cod_ons_3 <= "Q99" ~ "Congenital malformations, deformations and chromosomal abnormalities"
                                 , cod_ons_3 >= "V01" & cod_ons_3 <= "V89" ~ "Land transport accidents"
                                 , cod_ons_3 >= "W00" & cod_ons_3 <= "W19" ~ "Accidental falls"
                                 , cod_ons_3 >= "W32" & cod_ons_3 <= "W34" ~ "Non-intentional firearm discharge"
                                 , cod_ons_3 >= "W65" & cod_ons_3 <= "W74" ~ "Accidental drowning and submersion"
                                 , cod_ons_3 >= "W75" & cod_ons_3 <= "W84" ~ "Accidental threats to breathing"
                                 , cod_ons_3 >= "X40" & cod_ons_3 <= "X49" ~ "Accidental poisoning"
                                 , (cod_ons_3 >= "X60" & cod_ons_3 <= "X84") | (cod_ons_3 >= "Y10" & cod_ons_3 <= "Y34") ~ "Suicide and injury/poisoning of undetermined intent"
                                 , cod_ons_4 == "U509" | (cod_ons_3 >= "X85" & cod_ons_3 <= "Y09") | cod_ons_4 == "Y871" ~ "Homicide and probable suicide"
                                 , cod_ons_3 >= "R00" & cod_ons_3 <= "R99" ~ "Symptoms, signs and ill-defined conditions"
                                 , cod_ons_4 %in% c("U071","U072", "U109") ~ "COVID-19"
                                 , TRUE ~ "All other causes")) %>%
  group_by(cohort, pod_ons_new, lcod_ons_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, total, proportion))

write_csv(deaths_cohort_pod_lcod, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_cohort_pod_lcod.csv"))

################################################################################

########## Create tables and compare to published ONS deaths ##########

# Quarterly (Mar 19 - Feb 21)

deaths_ons_quarter <- df_input %>%
  filter(study_month >= as_date("2019-03-01") & study_month <= as_date("2021-02-01")) %>%
  group_by(study_month, study_quarter) %>%
  summarise(deaths = n()) %>%
  left_join(read_csv(here::here("docs", "ons_comparison_data", "table1_sex_onsmortality.csv")) %>% 
              group_by(period) %>% 
              summarise(ons_deaths = sum(ons_deaths, na.rm = TRUE))
            , by = c("study_month" = "period")) %>%
  group_by(study_quarter) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)
            , ons_deaths = sum(ons_deaths, na.rm = TRUE)) %>%
  mutate(deaths = plyr::round_any(deaths, 10)) %>%
  mutate(proportion = deaths / ons_deaths)

write_csv(deaths_ons_quarter, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_quarter.csv"))

plot_deaths_ons_quarter <- ggplot(deaths_ons_quarter, aes(x = study_quarter, y = proportion)) +
  geom_line(size = 1, colour = "#9F67FF") +
  geom_point(fill = "#F4F4F4", colour = "#9F67FF", shape = 21, size = 1.5, stroke = 1.3) +
  labs(x = "Study quarter", y = "Percent of ONS deaths") +
  scale_x_continuous(expand = c(0, 1), breaks = seq(1, 8, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, 0.1)
                     , labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
  NT_style()

ggsave(plot = plot_deaths_ons_quarter, filename ="deaths_ons_quarter.png", path = here::here("output", "describe_cohorts", "ons_death_comparisons"), height = 10, width = 13.7, units = "cm", dpi = 600)

# Quarterly (Mar 19 - Feb 21) deaths by region 

deaths_ons_quarter_region <- df_input %>%
  filter(study_month >= as_date("2019-03-01") & study_month <= as_date("2021-02-01")) %>%
  group_by(study_month, region, study_quarter) %>%
  summarise(deaths = n()) %>%
  left_join(read_csv(here::here("docs", "ons_comparison_data", "region_onsmortality.csv"))
            , by = c("study_month" = "period", "region")) %>%
  group_by(study_quarter, region) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)
            , ons_deaths = sum(ons_deaths, na.rm = TRUE)) %>%
  mutate(deaths = plyr::round_any(deaths, 10)) %>%
  mutate(proportion = deaths / ons_deaths)

write_csv(deaths_ons_quarter_region, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_quarter_region.csv"))

deaths_ons_quarter_region_late <- df_input %>%
  filter(study_month >= as_date("2019-03-01") & study_month <= as_date("2021-02-01")) %>%
  group_by(study_month, region, study_quarter) %>%
  summarise(deaths = n()) %>%
  left_join(read_csv(here::here("docs", "ons_comparison_data", "region_onsmortality.csv"))
            , by = c("study_month" = "period", "region")) %>%
  group_by(study_quarter, region) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)
            , ons_deaths = sum(ons_deaths, na.rm = TRUE)) %>%
  mutate(deaths = plyr::round_any(deaths, 10)) %>%
  mutate(proportion = deaths / ons_deaths) %>% 
  filter(study_quarter >= 5)

write_csv(deaths_ons_quarter_region_late, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_quarter_region_late.csv"))

plot_deaths_ons_quarter_region <- ggplot(deaths_ons_quarter_region %>% filter(str_detect(region, "^E12"))
                                         , aes(x = study_quarter, y = proportion, colour = region)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  labs(x = "Study quarter", y = "Percent of ONS deaths") +
  scale_colour_NT(palette = NT_palette()) +
  scale_x_continuous(expand = c(0, 0.5), breaks = seq(1, 8, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, 0.1)
                     , labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
  NT_style()

ggsave(plot = plot_deaths_ons_quarter_region, filename ="deaths_ons_quarter_region.png", path = here::here("output", "describe_cohorts", "ons_death_comparisons"), height = 10, width = 13.7, units = "cm", dpi = 600)

# Quarterly (Mar 19 - Feb 21) deaths by sex - Table 1

deaths_ons_quarter_sex <- df_input %>%
  filter(study_month >= as_date("2019-03-01") & study_month <= as_date("2021-02-01")) %>%
  group_by(study_month, sex, study_quarter) %>%
  summarise(deaths = n()) %>%
  left_join(read_csv(here::here("docs", "ons_comparison_data", "table1_sex_onsmortality.csv"))
            , by = c("study_month" = "period", "sex")) %>%
  group_by(study_quarter, sex) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)
            , ons_deaths = sum(ons_deaths, na.rm = TRUE)) %>%
  mutate(deaths = plyr::round_any(deaths, 10)) %>%
  mutate(proportion = deaths / ons_deaths)

write_csv(deaths_ons_quarter_sex, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_quarter_sex.csv"))

plot_deaths_ons_quarter_sex <- ggplot(deaths_ons_quarter_sex %>% filter(sex %in% c("M", "F")), aes(x = study_quarter, y = proportion, colour = sex)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  labs(x = "Study quarter", y = "Percent of ONS deaths") +
  scale_colour_NT(palette = NT_palette()) +
  scale_x_continuous(expand = c(0, 1), breaks = seq(1, 8, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, 0.1)
                     , labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
  NT_style()

ggsave(plot = plot_deaths_ons_quarter_sex, filename ="deaths_ons_quarter_sex.png", path = here::here("output", "describe_cohorts", "ons_death_comparisons"), height = 10, width = 13.7, units = "cm", dpi = 600)

# Quarterly deaths (Mar 19 - Feb 21) by age group (<75, 75-79, 80-84, 85-89, 90+)  - Table 4, 8c

deaths_ons_quarter_agegrp <- df_input %>%
  filter(study_month >= as_date("2019-03-01") & study_month <= as_date("2021-02-01")) %>%
  mutate(agegrp_ons = case_when(age >= 0 & age <= 74 ~ "<75"
                            , age >= 75 & age <= 79 ~ "75-79"
                            , age >= 80 & age <= 84 ~ "80-84"
                            , age >= 85 & age <= 89 ~ "85-89"
                            , age >= 90 ~ "90+"
                            , TRUE ~ NA_character_)) %>%
  group_by(study_month, agegrp_ons, study_quarter) %>%
  summarise(deaths = n()) %>%
  left_join(read_csv(here::here("docs", "ons_comparison_data", "table4_8c_agegrp_onsmortality.csv"))
            , by = c("study_month" = "period", "agegrp_ons" = "agegrp"))  %>%
  group_by(study_quarter, agegrp_ons) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)
            , ons_deaths = sum(ons_deaths, na.rm = TRUE)) %>%
  mutate(deaths = plyr::round_any(deaths, 10)) %>%
  mutate(proportion = deaths / ons_deaths)

write_csv(deaths_ons_quarter_agegrp, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_quarter_agegrp.csv"))

plot_deaths_ons_quarter_agegrp <- ggplot(deaths_ons_quarter_agegrp %>% filter(!is.na(agegrp_ons)), aes(x = study_quarter, y = proportion, colour = agegrp_ons)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  labs(x = "Study quarter", y = "Percent of ONS deaths") +
  scale_colour_NT(palette = NT_palette()) +
  scale_x_continuous(expand = c(0, 1), breaks = seq(1, 8, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, 0.1)
                     , labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
  NT_style()

ggsave(plot = plot_deaths_ons_quarter_agegrp, filename ="deaths_ons_quarter_agegrp.png", path = here::here("output", "describe_cohorts", "ons_death_comparisons"), height = 10, width = 13.7, units = "cm", dpi = 600)

# Quarterly deaths (Mar 19 - Feb 21) by leading (top 10) cause of death - Table 11a
# Deaths for 2019 are by date of occurrence

# ONS cause of death groupings
# Check the categories mutually exclusive particularly around covid-19 addition
# Check how 4+ character codes appear - with or without "."
deaths_ons_quarter_lcod <- df_input %>%
  filter(study_month >= as_date("2019-03-01") & study_month <= as_date("2021-02-01")) %>%
  mutate(lcod_ons_grp = case_when(cod_ons_3 >= "A00" & cod_ons_3 <= "A09" ~ "Intestinal infectious diseases"
                                 , (cod_ons_3 >= "A15" & cod_ons_3 <= "A19") | cod_ons_3 == "B90" ~ "Tuberculosis"
                                 , cod_ons_3 %in% c("A20", "A44") | (cod_ons_3 >= "A75" & cod_ons_3 <= "A79") | (cod_ons_3 >= "A82" & cod_ons_3 <= "A84") | cod_ons_4 == "A852" | (cod_ons_3 >= "A90" & cod_ons_3 <= "A98") | (cod_ons_3 >= "B50" & cod_ons_3 <= "B57") ~ "Vector-borne diseases and rabies"
                                 , (cod_ons_3 >= "A33" & cod_ons_3 <= "A37") | cod_ons_4 == "A492" | cod_ons_3 %in% c("A80", "B01", "B02", "B05", "B06", "B15", "B16") | cod_ons_4 %in% c("B170", "B180", "B181") | cod_ons_3 %in% c("B26", "B91", "G14") ~ "Vaccine-preventable diseases"
                                 , cod_ons_3 %in% c("A39", "A87") | (cod_ons_3 >= "G00" & cod_ons_3 <= "G03") ~ "Meningitis and meningococcal infection"
                                 , cod_ons_3 >= "A40" & cod_ons_3 <= "A41" ~ "Septicaemia"
                                 , cod_ons_3 >= "B20" & cod_ons_3 <= "B24" ~ "HIV"
                                 , cod_ons_3 == "C15" ~ "Malignant neoplasm of oesophagus"
                                 , cod_ons_3 == "C16" ~ "Malignant neoplasm of stomach"
                                 , cod_ons_3 >= "C18" & cod_ons_3 <= "C21" ~ "Malignant neoplasm of colon, sigmoid, rectum and anus"
                                 , cod_ons_3 == "C22" ~ "Malignant neoplasm of liver and intrahepatic bile ducts"
                                 , cod_ons_3 >= "C23" & cod_ons_3 <= "C24" ~ "Malignant neoplasm of gallbladder and other parts of biliary tract"
                                 , cod_ons_3 == "C25" ~ "Malignant neoplasm of pancreas"
                                 , cod_ons_3 == "C32" ~ "Malignant neoplasm of larynx"
                                 , cod_ons_3 >= "C33" & cod_ons_3 <= "C34" ~ "Malignant neoplasm of trachea, bronchus and lung"
                                 , cod_ons_3 >= "C40" & cod_ons_3 <= "C41" ~ "Malignant neoplasm of bone and articular cartilage"
                                 , cod_ons_3 >= "C43" & cod_ons_3 <= "C44" ~ "Melanoma and other malignant neoplasms of skin"
                                 , cod_ons_3 == "C50" ~ "Malignant neoplasm of breast"
                                 , cod_ons_3 >= "C53" & cod_ons_3 <= "C55" ~ "Malignant neoplasm of uterus"
                                 , cod_ons_3 == "C56" ~ "Malignant neoplasm of ovary"
                                 , cod_ons_3 == "C61" ~ "Malignant neoplasm of prostate"
                                 , cod_ons_3 == "C64" ~ "Malignant neoplasm of kidney, except renal pelvis"
                                 , cod_ons_3 == "C67" ~ "Malignant neoplasm of bladder"
                                 , cod_ons_3 == "C71" ~ "Malignant neoplasm of brain"
                                 , cod_ons_3 >= "C81" & cod_ons_3 <= "C96" ~ "Malignant neoplasms, stated or presumed to be primary of lymphoid, haematopoietic and related tissue"
                                 , cod_ons_3 >= "D00" & cod_ons_3 <= "D48" ~ "In situ and benign neoplasms, and neoplasms of uncertain or unknown behaviour"
                                 , cod_ons_3 >= "E10" & cod_ons_3 <= "E14" ~ "Diabetes"
                                 , (cod_ons_3 >= "D50" & cod_ons_3 <= "D53") | (cod_ons_3 >= "E40" & cod_ons_3 <= "E64") ~ "Malnutrition, nutritional anaemias and other nutritional deficiencies"
                                 , cod_ons_3 >= "E86" & cod_ons_3 <= "E87" ~ "Disorders of fluid, electrolyte and acid-base balance"
                                 , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer disease"
                                 , cod_ons_3 >= "F10" & cod_ons_3 <= "F19" ~ "Mental and behavioural disorders due to psychoactive substance use"
                                 , cod_ons_3 >= "G10" & cod_ons_3 <= "G12" ~ "Systemic atrophies primarily affecting the central nervous system"
                                 , cod_ons_3 == "G20" ~ "Parkinson disease"
                                 , cod_ons_3 >= "G40" & cod_ons_3 <= "G41" ~ "Epilepsy and status epilepticus"
                                 , cod_ons_3 >= "G80" & cod_ons_3 <= "G83" ~ "Cerebral palsy and other paralytic syndromes"
                                 , cod_ons_3 >= "I05" & cod_ons_3 <= "I09" ~ "Chronic rheumatic heart diseases"
                                 , cod_ons_3 >= "I10" & cod_ons_3 <= "I15" ~ "Hypertensive diseases"
                                 , cod_ons_3 >= "I20" & cod_ons_3 <= "I25" ~ "Ischaemic heart diseases"
                                 , cod_ons_3 >= "I26" & cod_ons_3 <= "I28" ~ "Pulmonary heart disease and diseases of pulmonary circulation"
                                 , cod_ons_3 >= "I34" & cod_ons_3 <= "I38" ~ "Nonrheumatic valve disorders and endocarditis"
                                 , cod_ons_3 == "I42" ~ "Cardiomyopathy"
                                 , cod_ons_3 == "I46" ~ "Cardiac arrest"
                                 , cod_ons_3 >= "I47" & cod_ons_3 <= "I49" ~ "Cardiac arrhythmias"
                                 , cod_ons_3 >= "I50" & cod_ons_3 <= "I51" ~ "Heart failure and complications and ill-defined heart disease"
                                 , cod_ons_3 >= "I60" & cod_ons_3 <= "I69" ~ "Cerebrovascular diseases"
                                 , cod_ons_3 == "I70" ~ "Atherosclerosis"
                                 , cod_ons_3 == "I71" ~ "Aortic aneurysm and dissection"
                                 , (cod_ons_3 >= "J00" & cod_ons_3 <= "J06") | (cod_ons_3 >= "J20" & cod_ons_3 <= "J22") ~ "Acute respiratory infections other than influenza and pneumonia"
                                 , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Influenza and pneumonia"
                                 , cod_ons_3 >= "J40" & cod_ons_3 <= "J47" ~ "Chronic lower respiratory diseases"
                                 , cod_ons_3 >= "J80" & cod_ons_3 <= "J84" ~ "Pulmonary oedema and other interstitial pulmonary diseases"
                                 , cod_ons_3 == "J96" ~ "Respiratory failure"
                                 , (cod_ons_3 >= "K35" & cod_ons_3 <= "K46") | cod_ons_3 == "K56" ~ "Appendicitis, hernia and intestinal obstruction"
                                 , cod_ons_3 >= "K70" & cod_ons_3 <= "K76" ~ "Cirrhosis and other diseases of liver"
                                 , cod_ons_3 >= "M00" & cod_ons_3 <= "M99" ~ "Diseases of musculoskeletal system and connective tissue"
                                 , cod_ons_3 >= "N00" & cod_ons_3 <= "N39" ~ "Diseases of the urinary system"
                                 , cod_ons_3 >= "O00" & cod_ons_3 <= "O99" ~ "Pregnancy, childbirth and puerperium"
                                 , cod_ons_3 >= "P00" & cod_ons_3 <= "P96" ~ "Certain conditions originating in the perinatal period"
                                 , cod_ons_3 >= "Q00" & cod_ons_3 <= "Q99" ~ "Congenital malformations, deformations and chromosomal abnormalities"
                                 , cod_ons_3 >= "V01" & cod_ons_3 <= "V89" ~ "Land transport accidents"
                                 , cod_ons_3 >= "W00" & cod_ons_3 <= "W19" ~ "Accidental falls"
                                 , cod_ons_3 >= "W32" & cod_ons_3 <= "W34" ~ "Non-intentional firearm discharge"
                                 , cod_ons_3 >= "W65" & cod_ons_3 <= "W74" ~ "Accidental drowning and submersion"
                                 , cod_ons_3 >= "W75" & cod_ons_3 <= "W84" ~ "Accidental threats to breathing"
                                 , cod_ons_3 >= "X40" & cod_ons_3 <= "X49" ~ "Accidental poisoning"
                                 , (cod_ons_3 >= "X60" & cod_ons_3 <= "X84") | (cod_ons_3 >= "Y10" & cod_ons_3 <= "Y34") ~ "Suicide and injury/poisoning of undetermined intent"
                                 , cod_ons_4 == "U509" | (cod_ons_3 >= "X85" & cod_ons_3 <= "Y09") | cod_ons_4 == "Y871" ~ "Homicide and probable suicide"
                                 , cod_ons_3 >= "R00" & cod_ons_3 <= "R99" ~ "Symptoms, signs and ill-defined conditions"
                                 , cod_ons_4 %in% c("U071","U072", "U109") ~ "COVID-19"
                                 , TRUE ~ "All other causes")) %>%
  filter(lcod_ons_grp != "All other causes") %>%
  group_by(study_month, lcod_ons_grp, study_quarter) %>%
  summarise(deaths = n()) %>%
  mutate(lcod_ons_grp = tolower(lcod_ons_grp))  %>%
  left_join(read_csv(here::here("docs", "ons_comparison_data", "table11a_cod_onsmortality.csv"))
            , by = c("study_month" = "period", "lcod_ons_grp" = "cause"))  %>%
  group_by(study_quarter, lcod_ons_grp) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)
            , ons_deaths = sum(ons_deaths, na.rm = TRUE)) %>%
  arrange(study_quarter, desc(deaths)) %>%
  mutate(rank = row_number()) %>%
  arrange(study_quarter, desc(ons_deaths)) %>%
  mutate(rank_ons = row_number()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)) %>%
  mutate(proportion = deaths / ons_deaths) %>%
  arrange(study_quarter, rank)

write_csv(deaths_ons_quarter_lcod, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_quarter_lcod.csv"))

# Monthly deaths (Jan 20 - Feb 21) by place of death - Table 14a

deaths_ons_month_pod <- df_input %>%
  filter(study_month >= as_date("2020-01-01") & study_month <= as_date("2021-02-01")) %>%
  group_by(study_month, pod_ons) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)) %>%
  left_join(read_csv(here::here("docs", "ons_comparison_data", "table14a_pod_onsmortality.csv"))
            , by = c("study_month" = "period", "pod_ons" = "place_of_death")) %>%
  mutate(proportion = deaths / ons_deaths)

write_csv(deaths_ons_month_pod, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_month_pod.csv"))

plot_deaths_ons_month_pod <- ggplot(deaths_ons_month_pod %>% filter(pod_ons %in% c("Hospital", "Care home", "Home", "Hospice", "Elsewhere", "Other communal establishment"))
                                    , aes(x = study_month, y = proportion, colour = pod_ons)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  labs(x = "Month", y = "Percent of ONS deaths") +
  scale_colour_NT(palette = NT_palette()) +
  scale_x_date(expand = c(0, 1), date_breaks = "month", date_labels = "%b %y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, 0.1)
                     , labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
  NT_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(plot = plot_deaths_ons_month_pod, filename ="deaths_ons_month_pod.png", path = here::here("output", "describe_cohorts", "ons_death_comparisons"), height = 10, width = 13.7, units = "cm", dpi = 600)

################################################################################

########## Number of deaths by cohort and characteristic ##########

# Ratio - place of death

death_ratio_pod <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_pod, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_pod.csv"))

# Ratio - cause of death

death_ratio_cod <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, codgrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_cod, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_cod.csv"))

# Ratio - sex

death_ratio_sex <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, sex) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_sex, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_sex.csv"))

#  Ratio - age group

death_ratio_agegrp <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, agegrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_agegrp, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_agegrp.csv"))

#  Ratio - ethnicity

death_ratio_ethnicity <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, ethnicity) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_ethnicity, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_ethnicity.csv"))

#  Ratio - ethnicity GP

death_ratio_ethnicity_gp <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, ethnicity_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_ethnicity_gp, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_ethnicity_gp.csv"))

#  Ratio - ethnicity SUS

death_ratio_ethnicity_sus <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, ethnicity_sus) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_ethnicity_sus, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_ethnicity_sus.csv"))

#  Ratio - long term conditions

death_ratio_ltc <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, ltcgrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_ltc, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_ltc.csv"))

#  Ratio - palliative care

death_ratio_palcare <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, palcare) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_palcare, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_palcare.csv"))

#  Ratio - no palliative care

death_ratio_nopalcare <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, nopalcare) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_nopalcare, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_nopalcare.csv"))

#  Ratio - care home type

death_ratio_carehome <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, carehome) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_carehome, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_carehome.csv"))

# Ratio - Region

death_ratio_region <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, region) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_region, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_region.csv"))

# Ratio - Deprivation quintile

death_ratio_imd <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, imd_quintile) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_imd, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_imd.csv"))

# Ratio - Local authority deprivation quintile

death_ratio_imd_la <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, imd_quintile_la) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_imd_la, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_imd_la.csv"))

# Ratio - Rural urban

death_ratio_rural_urban <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, rural_urban) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_rural_urban, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_rural_urban.csv"))

# Ratio - GP Region

death_ratio_region_gp <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, region_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_region_gp, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_region_gp.csv"))

# Ratio - Local authority deprivation quintile gp

death_ratio_imd_la_gp <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, imd_quintile_la_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_imd_la_gp, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_imd_la_gp.csv"))

################################################################################

########## Descriptive table of cohorts ##########

cohorts_summary_table <- deaths_cohort %>% 
  pivot_wider(names_from = cohort, names_prefix = "deaths_cohort_", values_from = deaths) %>% 
  mutate(variable = "n") %>% 
  bind_rows(death_ratio_sex %>% 
              mutate(variable = "Sex"
                     , category = sex) %>%
              bind_rows(death_ratio_agegrp %>%  
                          mutate(variable = "Age group"
                                 , category = agegrp)) %>% 
              bind_rows(death_ratio_ethnicity %>%  
                          mutate(variable = "Ethnicity"
                                 , category = ethnicity)) %>% 
              bind_rows(death_ratio_pod %>%  
                          mutate(variable = "Place of death"
                                 , category = pod_ons_new)) %>% 
              bind_rows(death_ratio_cod %>%  
                          mutate(variable = "Cause of death"
                                 , category = codgrp)) %>% 
              bind_rows(death_ratio_ltc %>%  
                          mutate(variable = "Long term conditions"
                                 , category = ltcgrp)) %>%
              bind_rows(death_ratio_palcare %>%  
                          mutate(variable = "Palliative care"
                                 , category = case_when(palcare == TRUE ~ "Yes"
                                                        , TRUE ~ "No"))) %>%
              bind_rows(death_ratio_nopalcare %>%  
                          mutate(variable = "No palliative care"
                                 , category = case_when(nopalcare == TRUE ~ "Yes"
                                                        , TRUE ~ "No"))) %>%
              bind_rows(death_ratio_carehome %>%  
                          mutate(variable = "Care home"
                                 , category = carehome)) %>%
              bind_rows(death_ratio_region %>%  
                          mutate(variable = "Region"
                                 , category = region)) %>%
              bind_rows(death_ratio_imd %>%  
                          mutate(variable = "IMD quintile"
                                 , category = as.character(imd_quintile))) %>% 
              bind_rows(death_ratio_imd_la %>%  
                          mutate(variable = "LA IMD quintile"
                                 , category = as.character(imd_quintile_la))) %>% 
              bind_rows(death_ratio_rural_urban %>%  
                          mutate(variable = "Rural urban"
                                 , category = rural_urban)) %>% 
              mutate(percent_cohort_0 = round(proportion_cohort_0 * 100, 1)
                     , percent_cohort_1 = round(proportion_cohort_1 * 100, 1))) %>% 
  select(variable, category, deaths_cohort_0, deaths_cohort_1, percent_cohort_0, percent_cohort_1) %>% 
  arrange(factor(variable, levels = c("n", "Sex", "Age group", "Ethnicity", "Cause of death", "Long term conditions", "Palliative care"
                                      , "No palliative care", "Care home", "Region", "IMD quintile", "LA IMD quintile", "Rural urban")), category)

write_csv(cohorts_summary_table, here::here("output", "describe_cohorts", "cohorts_summary_table.csv"))

################################################################################

########## Number of deaths by quarter and characteristics ##########

# Place of death

deaths_quarter_pod <- df_input %>%
  group_by(study_quarter, pod_ons_new) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod.csv"))

# Cause of death

deaths_quarter_cod <- df_input %>%
  group_by(study_quarter, codgrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_cod, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_cod.csv"))

# Leading causes of death

deaths_quarter_leading_cod <- df_input %>%
  mutate(lcod_ons_grp = case_when(cod_ons_3 >= "A00" & cod_ons_3 <= "A09" ~ "Intestinal infectious diseases"
                                 , (cod_ons_3 >= "A15" & cod_ons_3 <= "A19") | cod_ons_3 == "B90" ~ "Tuberculosis"
                                 , cod_ons_3 %in% c("A20", "A44") | (cod_ons_3 >= "A75" & cod_ons_3 <= "A79") | (cod_ons_3 >= "A82" & cod_ons_3 <= "A84") | cod_ons_4 == "A852" | (cod_ons_3 >= "A90" & cod_ons_3 <= "A98") | (cod_ons_3 >= "B50" & cod_ons_3 <= "B57") ~ "Vector-borne diseases and rabies"
                                 , (cod_ons_3 >= "A33" & cod_ons_3 <= "A37") | cod_ons_4 == "A492" | cod_ons_3 %in% c("A80", "B01", "B02", "B05", "B06", "B15", "B16") | cod_ons_4 %in% c("B170", "B180", "B181") | cod_ons_3 %in% c("B26", "B91", "G14") ~ "Vaccine-preventable diseases"
                                 , cod_ons_3 %in% c("A39", "A87") | (cod_ons_3 >= "G00" & cod_ons_3 <= "G03") ~ "Meningitis and meningococcal infection"
                                 , cod_ons_3 >= "A40" & cod_ons_3 <= "A41" ~ "Septicaemia"
                                 , cod_ons_3 >= "B20" & cod_ons_3 <= "B24" ~ "HIV"
                                 , cod_ons_3 == "C15" ~ "Malignant neoplasm of oesophagus"
                                 , cod_ons_3 == "C16" ~ "Malignant neoplasm of stomach"
                                 , cod_ons_3 >= "C18" & cod_ons_3 <= "C21" ~ "Malignant neoplasm of colon, sigmoid, rectum and anus"
                                 , cod_ons_3 == "C22" ~ "Malignant neoplasm of liver and intrahepatic bile ducts"
                                 , cod_ons_3 >= "C23" & cod_ons_3 <= "C24" ~ "Malignant neoplasm of gallbladder and other parts of biliary tract"
                                 , cod_ons_3 == "C25" ~ "Malignant neoplasm of pancreas"
                                 , cod_ons_3 == "C32" ~ "Malignant neoplasm of larynx"
                                 , cod_ons_3 >= "C33" & cod_ons_3 <= "C34" ~ "Malignant neoplasm of trachea, bronchus and lung"
                                 , cod_ons_3 >= "C40" & cod_ons_3 <= "C41" ~ "Malignant neoplasm of bone and articular cartilage"
                                 , cod_ons_3 >= "C43" & cod_ons_3 <= "C44" ~ "Melanoma and other malignant neoplasms of skin"
                                 , cod_ons_3 == "C50" ~ "Malignant neoplasm of breast"
                                 , cod_ons_3 >= "C53" & cod_ons_3 <= "C55" ~ "Malignant neoplasm of uterus"
                                 , cod_ons_3 == "C56" ~ "Malignant neoplasm of ovary"
                                 , cod_ons_3 == "C61" ~ "Malignant neoplasm of prostate"
                                 , cod_ons_3 == "C64" ~ "Malignant neoplasm of kidney, except renal pelvis"
                                 , cod_ons_3 == "C67" ~ "Malignant neoplasm of bladder"
                                 , cod_ons_3 == "C71" ~ "Malignant neoplasm of brain"
                                 , cod_ons_3 >= "C81" & cod_ons_3 <= "C96" ~ "Malignant neoplasms, stated or presumed to be primary of lymphoid, haematopoietic and related tissue"
                                 , cod_ons_3 >= "D00" & cod_ons_3 <= "D48" ~ "In situ and benign neoplasms, and neoplasms of uncertain or unknown behaviour"
                                 , cod_ons_3 >= "E10" & cod_ons_3 <= "E14" ~ "Diabetes"
                                 , (cod_ons_3 >= "D50" & cod_ons_3 <= "D53") | (cod_ons_3 >= "E40" & cod_ons_3 <= "E64") ~ "Malnutrition, nutritional anaemias and other nutritional deficiencies"
                                 , cod_ons_3 >= "E86" & cod_ons_3 <= "E87" ~ "Disorders of fluid, electrolyte and acid-base balance"
                                 , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer disease"
                                 , cod_ons_3 >= "F10" & cod_ons_3 <= "F19" ~ "Mental and behavioural disorders due to psychoactive substance use"
                                 , cod_ons_3 >= "G10" & cod_ons_3 <= "G12" ~ "Systemic atrophies primarily affecting the central nervous system"
                                 , cod_ons_3 == "G20" ~ "Parkinson disease"
                                 , cod_ons_3 >= "G40" & cod_ons_3 <= "G41" ~ "Epilepsy and status epilepticus"
                                 , cod_ons_3 >= "G80" & cod_ons_3 <= "G83" ~ "Cerebral palsy and other paralytic syndromes"
                                 , cod_ons_3 >= "I05" & cod_ons_3 <= "I09" ~ "Chronic rheumatic heart diseases"
                                 , cod_ons_3 >= "I10" & cod_ons_3 <= "I15" ~ "Hypertensive diseases"
                                 , cod_ons_3 >= "I20" & cod_ons_3 <= "I25" ~ "Ischaemic heart diseases"
                                 , cod_ons_3 >= "I26" & cod_ons_3 <= "I28" ~ "Pulmonary heart disease and diseases of pulmonary circulation"
                                 , cod_ons_3 >= "I34" & cod_ons_3 <= "I38" ~ "Nonrheumatic valve disorders and endocarditis"
                                 , cod_ons_3 == "I42" ~ "Cardiomyopathy"
                                 , cod_ons_3 == "I46" ~ "Cardiac arrest"
                                 , cod_ons_3 >= "I47" & cod_ons_3 <= "I49" ~ "Cardiac arrhythmias"
                                 , cod_ons_3 >= "I50" & cod_ons_3 <= "I51" ~ "Heart failure and complications and ill-defined heart disease"
                                 , cod_ons_3 >= "I60" & cod_ons_3 <= "I69" ~ "Cerebrovascular diseases"
                                 , cod_ons_3 == "I70" ~ "Atherosclerosis"
                                 , cod_ons_3 == "I71" ~ "Aortic aneurysm and dissection"
                                 , (cod_ons_3 >= "J00" & cod_ons_3 <= "J06") | (cod_ons_3 >= "J20" & cod_ons_3 <= "J22") ~ "Acute respiratory infections other than influenza and pneumonia"
                                 , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Influenza and pneumonia"
                                 , cod_ons_3 >= "J40" & cod_ons_3 <= "J47" ~ "Chronic lower respiratory diseases"
                                 , cod_ons_3 >= "J80" & cod_ons_3 <= "J84" ~ "Pulmonary oedema and other interstitial pulmonary diseases"
                                 , cod_ons_3 == "J96" ~ "Respiratory failure"
                                 , (cod_ons_3 >= "K35" & cod_ons_3 <= "K46") | cod_ons_3 == "K56" ~ "Appendicitis, hernia and intestinal obstruction"
                                 , cod_ons_3 >= "K70" & cod_ons_3 <= "K76" ~ "Cirrhosis and other diseases of liver"
                                 , cod_ons_3 >= "M00" & cod_ons_3 <= "M99" ~ "Diseases of musculoskeletal system and connective tissue"
                                 , cod_ons_3 >= "N00" & cod_ons_3 <= "N39" ~ "Diseases of the urinary system"
                                 , cod_ons_3 >= "O00" & cod_ons_3 <= "O99" ~ "Pregnancy, childbirth and puerperium"
                                 , cod_ons_3 >= "P00" & cod_ons_3 <= "P96" ~ "Certain conditions originating in the perinatal period"
                                 , cod_ons_3 >= "Q00" & cod_ons_3 <= "Q99" ~ "Congenital malformations, deformations and chromosomal abnormalities"
                                 , cod_ons_3 >= "V01" & cod_ons_3 <= "V89" ~ "Land transport accidents"
                                 , cod_ons_3 >= "W00" & cod_ons_3 <= "W19" ~ "Accidental falls"
                                 , cod_ons_3 >= "W32" & cod_ons_3 <= "W34" ~ "Non-intentional firearm discharge"
                                 , cod_ons_3 >= "W65" & cod_ons_3 <= "W74" ~ "Accidental drowning and submersion"
                                 , cod_ons_3 >= "W75" & cod_ons_3 <= "W84" ~ "Accidental threats to breathing"
                                 , cod_ons_3 >= "X40" & cod_ons_3 <= "X49" ~ "Accidental poisoning"
                                 , (cod_ons_3 >= "X60" & cod_ons_3 <= "X84") | (cod_ons_3 >= "Y10" & cod_ons_3 <= "Y34") ~ "Suicide and injury/poisoning of undetermined intent"
                                 , cod_ons_4 == "U509" | (cod_ons_3 >= "X85" & cod_ons_3 <= "Y09") | cod_ons_4 == "Y871" ~ "Homicide and probable suicide"
                                 , cod_ons_3 >= "R00" & cod_ons_3 <= "R99" ~ "Symptoms, signs and ill-defined conditions"
                                 , cod_ons_4 %in% c("U071","U072", "U109") ~ "COVID-19"
                                 , TRUE ~ "All other causes")) %>%
  group_by(study_quarter, lcod_ons_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_leading_cod, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_leading_cod.csv"))

# Sex

deaths_quarter_sex <- df_input %>%
  group_by(study_quarter, sex) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_sex, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_sex.csv"))

# Age group

deaths_quarter_agegrp <- df_input %>%
  group_by(study_quarter, agegrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_agegrp, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_agegrp.csv"))

# Ethnicity

deaths_quarter_ethnicity <- df_input %>%
  group_by(study_quarter, ethnicity) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_ethnicity, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_ethnicity.csv"))

# Ethnicity GP

deaths_quarter_ethnicity_gp <- df_input %>%
  group_by(study_quarter, ethnicity_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_ethnicity_gp, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_ethnicity_gp.csv"))

# Ethnicity SUS

deaths_quarter_ethnicity_sus <- df_input %>%
  group_by(study_quarter, ethnicity_sus) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_ethnicity_sus, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_ethnicity_sus.csv"))

#  Long term conditions

deaths_quarter_ltc <- df_input %>%
  group_by(study_quarter, ltcgrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_ltc, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_ltc.csv"))

#  Palliative care

deaths_quarter_palcare <- df_input %>%
  group_by(study_quarter, palcare) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))
write_csv(deaths_quarter_palcare, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_palcare.csv"))

#  No palliative care

deaths_quarter_nopalcare <- df_input %>%
  group_by(study_quarter, nopalcare) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))
write_csv(deaths_quarter_nopalcare, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_nopalcare.csv"))

#  Care home

deaths_quarter_carehome <- df_input %>%
  group_by(study_quarter, carehome) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))
write_csv(deaths_quarter_carehome, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_carehome.csv"))

# MSOA

deaths_quarter_msoa <- df_input %>%
  mutate(msoa_present = case_when(is.na(msoa) ~ "No"
                                  , TRUE ~ "Yes")) %>%
  group_by(study_quarter, msoa_present) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_msoa, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_msoa.csv"))

# Region

deaths_quarter_region <- df_input %>%
  group_by(study_quarter, region) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_region, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_region.csv"))

# Deprivation quintile

deaths_quarter_imd <- df_input %>%
  group_by(study_quarter, imd_quintile) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_imd, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_imd.csv"))

# Local authority deprivation quintile

deaths_quarter_imd_la <- df_input %>%
  group_by(study_quarter, imd_quintile_la) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_imd_la, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_imd_la.csv"))

# Rural urban

deaths_quarter_rural_urban <- df_input %>%
  group_by(study_quarter, rural_urban) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_rural_urban, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_rural_urban.csv"))

# GP Region

deaths_quarter_region_gp <- df_input %>%
  group_by(study_quarter, region_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_region_gp, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_region_gp.csv"))

# GP Local authority deprivation quintile

deaths_quarter_imd_la_gp <- df_input %>%
  group_by(study_quarter, imd_quintile_la_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_imd_la_gp, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_imd_la_gp.csv"))

################################################################################

########## Descriptive table of quarters ##########

quarter_summary_table <- deaths_quarter %>% 
  pivot_wider(names_from = study_quarter, names_prefix = "deaths_study_quarter_", values_from = deaths) %>% 
  mutate(variable = "n") %>% 
  bind_rows(deaths_quarter_sex %>% 
              mutate(variable = "Sex"
                     , category = sex) %>%
              bind_rows(deaths_quarter_agegrp %>%  
                          mutate(variable = "Age group"
                                 , category = agegrp)) %>% 
              bind_rows(deaths_quarter_ethnicity %>%  
                          mutate(variable = "Ethnicity"
                                 , category = ethnicity)) %>% 
              bind_rows(deaths_quarter_pod %>%  
                          mutate(variable = "Place of death"
                                 , category = pod_ons_new)) %>% 
              bind_rows(deaths_quarter_cod %>%  
                          mutate(variable = "Cause of death"
                                 , category = codgrp)) %>% 
              bind_rows(deaths_quarter_ltc %>%  
                          mutate(variable = "Long term conditions"
                                 , category = ltcgrp)) %>%
              bind_rows(deaths_quarter_palcare %>%  
                          mutate(variable = "Palliative care"
                                 , category = case_when(palcare == TRUE ~ "Yes"
                                                        , TRUE ~ "No"))) %>%
              bind_rows(deaths_quarter_nopalcare %>%  
                          mutate(variable = "No palliative care"
                                 , category = case_when(nopalcare == TRUE ~ "Yes"
                                                        , TRUE ~ "No"))) %>%
              bind_rows(deaths_quarter_carehome %>%  
                          mutate(variable = "Care home"
                                 , category = carehome)) %>%
              bind_rows(deaths_quarter_region %>%  
                          mutate(variable = "Region"
                                 , category = region)) %>%
              bind_rows(deaths_quarter_imd %>%  
                          mutate(variable = "IMD quintile"
                                 , category = as.character(imd_quintile))) %>% 
              bind_rows(deaths_quarter_imd_la %>%  
                          mutate(variable = "LA IMD quintile"
                                 , category = as.character(imd_quintile_la))) %>% 
              bind_rows(deaths_quarter_rural_urban %>%  
                          mutate(variable = "Rural urban"
                                 , category = rural_urban)) %>% 
              mutate(percent_study_quarter_1 = round(proportion_study_quarter_1 * 100, 1)
                     , percent_study_quarter_2 = round(proportion_study_quarter_2 * 100, 1)
                     , percent_study_quarter_3 = round(proportion_study_quarter_3 * 100, 1)
                     , percent_study_quarter_4 = round(proportion_study_quarter_4 * 100, 1)
                     , percent_study_quarter_5 = round(proportion_study_quarter_5 * 100, 1)
                     , percent_study_quarter_6 = round(proportion_study_quarter_6 * 100, 1)
                     , percent_study_quarter_7 = round(proportion_study_quarter_7 * 100, 1)
                     , percent_study_quarter_8 = round(proportion_study_quarter_8 * 100, 1))) %>% 
  select(variable, category, starts_with("deaths_study_quarter_"), starts_with("percent_study_quarter_")) %>% 
  arrange(factor(variable, levels = c("n", "Sex", "Age group", "Ethnicity", "Cause of death", "Long term conditions", "Palliative care"
                                      , "No palliative care", "Care home", "Region", "IMD quintile", "LA IMD quintile", "Rural urban")), category)
write_csv(quarter_summary_table, here::here("output", "describe_cohorts", "quarter_summary_table.csv"))

################################################################################

########## Ratios of deaths by place of death for characteristics ##########

# Ratio - pod * cause of death

death_ratio_pod_cod <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, codgrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, codgrp)

write_csv(death_ratio_pod_cod, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_cod.csv"))

#  Ratio - pod * sex

death_ratio_pod_sex <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, sex) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, sex)

write_csv(death_ratio_pod_sex, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_sex.csv"))

#  Ratio - pod * age group

death_ratio_pod_agegrp <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, agegrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, agegrp)

write_csv(death_ratio_pod_agegrp, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_agegrp.csv"))

#  Ratio - pod * ethnicity

death_ratio_pod_ethnicity <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, ethnicity) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, ethnicity)

write_csv(death_ratio_pod_ethnicity, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_ethnicity.csv"))

#  Ratio - pod * ethnicity GP

death_ratio_pod_ethnicity_gp <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, ethnicity_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, ethnicity_gp)

write_csv(death_ratio_pod_ethnicity_gp, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_ethnicity_gp.csv"))

#  Ratio - pod * ethnicity SUS

death_ratio_pod_ethnicity_sus <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, ethnicity_sus) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, ethnicity_sus)

write_csv(death_ratio_pod_ethnicity_sus, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_ethnicity_sus.csv"))

#  Ratio - pod * long term conditions

death_ratio_pod_ltc <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, ltcgrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, ltcgrp)

write_csv(death_ratio_pod_ltc, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_ltc.csv"))

#  Ratio - pod * palliative care

death_ratio_pod_palcare <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, palcare) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, palcare)

write_csv(death_ratio_pod_palcare, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_palcare.csv"))

#  Ratio - pod * no palliative care

death_ratio_pod_nopalcare <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, nopalcare) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, nopalcare)

write_csv(death_ratio_pod_nopalcare, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_nopalcare.csv"))

#  Ratio - pod * care home

death_ratio_pod_carehome <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, carehome) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, carehome)

write_csv(death_ratio_pod_carehome, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_carehome.csv"))

# Ratio - pod * Region

death_ratio_pod_region <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, region) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, region)

write_csv(death_ratio_pod_region, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_region.csv"))

# Ratio - pod * Deprivation quintile

death_ratio_pod_imd <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, imd_quintile) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, imd_quintile)

write_csv(death_ratio_pod_imd, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_imd.csv"))

# Ratio - pod * Local authority deprivation quintile

death_ratio_pod_imd_la <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, imd_quintile_la) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, imd_quintile_la)

write_csv(death_ratio_pod_imd_la, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_imd_la.csv"))

# Ratio - pod * rural urban

death_ratio_pod_rural_urban <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, rural_urban) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, rural_urban)

write_csv(death_ratio_pod_rural_urban, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_rural_urban.csv"))

# Ratio - pod * Region gp

death_ratio_pod_region_gp <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, region_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, region_gp)

write_csv(death_ratio_pod_region_gp, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_region_gp.csv"))

# Ratio - pod * Local authority deprivation quintile GP

death_ratio_pod_imd_la_gp <- df_input %>%
  filter(!is.na(study_cohort)) %>% 
  group_by(cohort, pod_ons_new, imd_quintile_la_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons_new, imd_quintile_la_gp)

write_csv(death_ratio_pod_imd_la_gp, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_imd_la_gp.csv"))

################################################################################

########## Descriptive table of cohorts by place of death ##########

cohorts_pod_summary_table <- deaths_cohort_pod %>% 
  select(cohort, pod_ons_new, deaths) %>% 
  pivot_wider(names_from = cohort, names_prefix = "deaths_cohort_", values_from = deaths) %>% 
  mutate(variable = "n") %>% 
  bind_rows(death_ratio_pod_sex %>% 
              mutate(variable = "Sex"
                     , category = sex) %>%
              bind_rows(death_ratio_pod_agegrp %>%  
                          mutate(variable = "Age group"
                                 , category = agegrp)) %>% 
              bind_rows(death_ratio_pod_ethnicity %>%  
                          mutate(variable = "Ethnicity"
                                 , category = ethnicity)) %>% 
              bind_rows(death_ratio_pod_cod %>%  
                          mutate(variable = "Cause of death"
                                 , category = codgrp)) %>% 
              bind_rows(death_ratio_pod_ltc %>%  
                          mutate(variable = "Long term conditions"
                                 , category = ltcgrp)) %>%
              bind_rows(death_ratio_pod_palcare %>%  
                          mutate(variable = "Palliative care"
                                 , category = case_when(palcare == TRUE ~ "Yes"
                                                        , TRUE ~ "No"))) %>%
              bind_rows(death_ratio_pod_nopalcare %>%  
                          mutate(variable = "No palliative care"
                                 , category = case_when(nopalcare == TRUE ~ "Yes"
                                                        , TRUE ~ "No"))) %>%
              bind_rows(death_ratio_pod_carehome %>%  
                          mutate(variable = "Care home"
                                 , category = carehome)) %>%
              bind_rows(death_ratio_pod_region %>%  
                          mutate(variable = "Region"
                                 , category = region)) %>%
              bind_rows(death_ratio_pod_imd %>%  
                          mutate(variable = "IMD quintile"
                                 , category = as.character(imd_quintile))) %>% 
              bind_rows(death_ratio_pod_imd_la %>%  
                          mutate(variable = "LA IMD quintile"
                                 , category = as.character(imd_quintile_la))) %>% 
              bind_rows(death_ratio_pod_rural_urban %>%  
                          mutate(variable = "Rural urban"
                                 , category = rural_urban)) %>% 
              mutate(percent_cohort_0 = round(proportion_cohort_0 * 100, 1)
                     , percent_cohort_1 = round(proportion_cohort_1 * 100, 1))) %>% 
  select(pod_ons_new, variable, category, deaths_cohort_0, deaths_cohort_1, percent_cohort_0, percent_cohort_1) %>% 
  arrange(pod_ons_new, factor(variable, levels = c("n", "Sex", "Age group", "Ethnicity", "Cause of death", "Long term conditions", "Palliative care"
                                      , "No palliative care", "Care home", "Region", "IMD quintile", "LA IMD quintile", "Rural urban")), category)

write_csv(cohorts_pod_summary_table, here::here("output", "describe_cohorts", "cohorts_pod_summary_table.csv"))

cohorts_pod_summary_table_short <- deaths_cohort_pod %>% 
  select(cohort, pod_ons_new, deaths) %>% 
  pivot_wider(names_from = cohort, names_prefix = "deaths_cohort_", values_from = deaths) %>% 
  mutate(variable = "n") %>% 
  bind_rows(death_ratio_pod_sex %>% 
              mutate(variable = "Sex"
                     , category = sex) %>%
              filter(sex == "F") %>% 
              bind_rows(df_input %>%
                          filter(!is.na(study_cohort)) %>%
                          mutate(agegrp = case_when(age >= 0 & age <= 79 ~ "00-79"
                                                    , age >= 80 ~ "80+")) %>%
                          group_by(cohort, pod_ons_new, agegrp) %>%
                          summarise(deaths = n()) %>%
                          mutate(deaths = plyr::round_any(deaths, 10)
                                 , total = sum(deaths)
                                 , proportion = deaths / total) %>%
                          select(-total) %>%
                          pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
                          mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
                                 , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
                          arrange(pod_ons_new, agegrp)%>% 
                          mutate(variable = "Age 80+"
                                 , category = agegrp) %>%
                          filter(agegrp == "80+")) %>% 
              bind_rows(death_ratio_pod_ethnicity %>%  
                          mutate(variable = "Ethnicity"
                                 , category = ethnicity) %>% 
                          filter(ethnicity == 1)) %>% 
              bind_rows(death_ratio_pod_cod %>%  
                          mutate(variable = "Cause of death"
                                 , category = codgrp)) %>% 
              bind_rows(df_input %>%
                          filter(!is.na(study_cohort)) %>%
                          mutate(ltc_count = df_input %>%
                                   filter(!is.na(study_cohort)) %>% 
                                   select(starts_with("ltc_")) %>% rowSums()
                                 , ltc_grp = case_when(ltc_count < 3 ~ "<3"
                                                       , ltc_count >= 3 ~ "3+"
                                                       , TRUE ~ NA_character_)) %>%
                          group_by(cohort, pod_ons_new, ltc_grp) %>%
                          summarise(deaths = n()) %>%
                          mutate(deaths = plyr::round_any(deaths, 10)
                                 , total = sum(deaths)
                                 , proportion = deaths / total) %>%
                          select(-total) %>%
                          pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
                          mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
                                 , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
                          arrange(pod_ons_new, ltc_grp) %>%  
                          mutate(variable = "Long term conditions"
                                 , category = ltc_grp) %>%
                          filter(ltc_grp == "3+")) %>%
              mutate(percent_cohort_0 = round(proportion_cohort_0 * 100, 1)
                     , percent_cohort_1 = round(proportion_cohort_1 * 100, 1))) %>% 
  select(pod_ons_new, variable, category, deaths_cohort_0, deaths_cohort_1, percent_cohort_0, percent_cohort_1) %>% 
  arrange(pod_ons_new, factor(variable, levels = c("n", "Sex", "Age 80+", "Ethnicity", "Cause of death", "Long term conditions")), category)

write_csv(cohorts_pod_summary_table_short, here::here("output", "describe_cohorts", "cohorts_pod_summary_table_short.csv"))

################################################################################

########## Death counts by place of death, quarter and characteristics ##########

# Cause of death

deaths_quarter_pod_cod <- df_input %>%
  group_by(study_quarter, pod_ons_new, codgrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod_cod, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_cod.csv"))

# Leading causes of death

deaths_quarter_pod_leading_cod <- df_input %>%
  mutate(lcod_ons_grp = case_when(cod_ons_3 >= "A00" & cod_ons_3 <= "A09" ~ "Intestinal infectious diseases"
                                 , (cod_ons_3 >= "A15" & cod_ons_3 <= "A19") | cod_ons_3 == "B90" ~ "Tuberculosis"
                                 , cod_ons_3 %in% c("A20", "A44") | (cod_ons_3 >= "A75" & cod_ons_3 <= "A79") | (cod_ons_3 >= "A82" & cod_ons_3 <= "A84") | cod_ons_4 == "A852" | (cod_ons_3 >= "A90" & cod_ons_3 <= "A98") | (cod_ons_3 >= "B50" & cod_ons_3 <= "B57") ~ "Vector-borne diseases and rabies"
                                 , (cod_ons_3 >= "A33" & cod_ons_3 <= "A37") | cod_ons_4 == "A492" | cod_ons_3 %in% c("A80", "B01", "B02", "B05", "B06", "B15", "B16") | cod_ons_4 %in% c("B170", "B180", "B181") | cod_ons_3 %in% c("B26", "B91", "G14") ~ "Vaccine-preventable diseases"
                                 , cod_ons_3 %in% c("A39", "A87") | (cod_ons_3 >= "G00" & cod_ons_3 <= "G03") ~ "Meningitis and meningococcal infection"
                                 , cod_ons_3 >= "A40" & cod_ons_3 <= "A41" ~ "Septicaemia"
                                 , cod_ons_3 >= "B20" & cod_ons_3 <= "B24" ~ "HIV"
                                 , cod_ons_3 == "C15" ~ "Malignant neoplasm of oesophagus"
                                 , cod_ons_3 == "C16" ~ "Malignant neoplasm of stomach"
                                 , cod_ons_3 >= "C18" & cod_ons_3 <= "C21" ~ "Malignant neoplasm of colon, sigmoid, rectum and anus"
                                 , cod_ons_3 == "C22" ~ "Malignant neoplasm of liver and intrahepatic bile ducts"
                                 , cod_ons_3 >= "C23" & cod_ons_3 <= "C24" ~ "Malignant neoplasm of gallbladder and other parts of biliary tract"
                                 , cod_ons_3 == "C25" ~ "Malignant neoplasm of pancreas"
                                 , cod_ons_3 == "C32" ~ "Malignant neoplasm of larynx"
                                 , cod_ons_3 >= "C33" & cod_ons_3 <= "C34" ~ "Malignant neoplasm of trachea, bronchus and lung"
                                 , cod_ons_3 >= "C40" & cod_ons_3 <= "C41" ~ "Malignant neoplasm of bone and articular cartilage"
                                 , cod_ons_3 >= "C43" & cod_ons_3 <= "C44" ~ "Melanoma and other malignant neoplasms of skin"
                                 , cod_ons_3 == "C50" ~ "Malignant neoplasm of breast"
                                 , cod_ons_3 >= "C53" & cod_ons_3 <= "C55" ~ "Malignant neoplasm of uterus"
                                 , cod_ons_3 == "C56" ~ "Malignant neoplasm of ovary"
                                 , cod_ons_3 == "C61" ~ "Malignant neoplasm of prostate"
                                 , cod_ons_3 == "C64" ~ "Malignant neoplasm of kidney, except renal pelvis"
                                 , cod_ons_3 == "C67" ~ "Malignant neoplasm of bladder"
                                 , cod_ons_3 == "C71" ~ "Malignant neoplasm of brain"
                                 , cod_ons_3 >= "C81" & cod_ons_3 <= "C96" ~ "Malignant neoplasms, stated or presumed to be primary of lymphoid, haematopoietic and related tissue"
                                 , cod_ons_3 >= "D00" & cod_ons_3 <= "D48" ~ "In situ and benign neoplasms, and neoplasms of uncertain or unknown behaviour"
                                 , cod_ons_3 >= "E10" & cod_ons_3 <= "E14" ~ "Diabetes"
                                 , (cod_ons_3 >= "D50" & cod_ons_3 <= "D53") | (cod_ons_3 >= "E40" & cod_ons_3 <= "E64") ~ "Malnutrition, nutritional anaemias and other nutritional deficiencies"
                                 , cod_ons_3 >= "E86" & cod_ons_3 <= "E87" ~ "Disorders of fluid, electrolyte and acid-base balance"
                                 , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer disease"
                                 , cod_ons_3 >= "F10" & cod_ons_3 <= "F19" ~ "Mental and behavioural disorders due to psychoactive substance use"
                                 , cod_ons_3 >= "G10" & cod_ons_3 <= "G12" ~ "Systemic atrophies primarily affecting the central nervous system"
                                 , cod_ons_3 == "G20" ~ "Parkinson disease"
                                 , cod_ons_3 >= "G40" & cod_ons_3 <= "G41" ~ "Epilepsy and status epilepticus"
                                 , cod_ons_3 >= "G80" & cod_ons_3 <= "G83" ~ "Cerebral palsy and other paralytic syndromes"
                                 , cod_ons_3 >= "I05" & cod_ons_3 <= "I09" ~ "Chronic rheumatic heart diseases"
                                 , cod_ons_3 >= "I10" & cod_ons_3 <= "I15" ~ "Hypertensive diseases"
                                 , cod_ons_3 >= "I20" & cod_ons_3 <= "I25" ~ "Ischaemic heart diseases"
                                 , cod_ons_3 >= "I26" & cod_ons_3 <= "I28" ~ "Pulmonary heart disease and diseases of pulmonary circulation"
                                 , cod_ons_3 >= "I34" & cod_ons_3 <= "I38" ~ "Nonrheumatic valve disorders and endocarditis"
                                 , cod_ons_3 == "I42" ~ "Cardiomyopathy"
                                 , cod_ons_3 == "I46" ~ "Cardiac arrest"
                                 , cod_ons_3 >= "I47" & cod_ons_3 <= "I49" ~ "Cardiac arrhythmias"
                                 , cod_ons_3 >= "I50" & cod_ons_3 <= "I51" ~ "Heart failure and complications and ill-defined heart disease"
                                 , cod_ons_3 >= "I60" & cod_ons_3 <= "I69" ~ "Cerebrovascular diseases"
                                 , cod_ons_3 == "I70" ~ "Atherosclerosis"
                                 , cod_ons_3 == "I71" ~ "Aortic aneurysm and dissection"
                                 , (cod_ons_3 >= "J00" & cod_ons_3 <= "J06") | (cod_ons_3 >= "J20" & cod_ons_3 <= "J22") ~ "Acute respiratory infections other than influenza and pneumonia"
                                 , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Influenza and pneumonia"
                                 , cod_ons_3 >= "J40" & cod_ons_3 <= "J47" ~ "Chronic lower respiratory diseases"
                                 , cod_ons_3 >= "J80" & cod_ons_3 <= "J84" ~ "Pulmonary oedema and other interstitial pulmonary diseases"
                                 , cod_ons_3 == "J96" ~ "Respiratory failure"
                                 , (cod_ons_3 >= "K35" & cod_ons_3 <= "K46") | cod_ons_3 == "K56" ~ "Appendicitis, hernia and intestinal obstruction"
                                 , cod_ons_3 >= "K70" & cod_ons_3 <= "K76" ~ "Cirrhosis and other diseases of liver"
                                 , cod_ons_3 >= "M00" & cod_ons_3 <= "M99" ~ "Diseases of musculoskeletal system and connective tissue"
                                 , cod_ons_3 >= "N00" & cod_ons_3 <= "N39" ~ "Diseases of the urinary system"
                                 , cod_ons_3 >= "O00" & cod_ons_3 <= "O99" ~ "Pregnancy, childbirth and puerperium"
                                 , cod_ons_3 >= "P00" & cod_ons_3 <= "P96" ~ "Certain conditions originating in the perinatal period"
                                 , cod_ons_3 >= "Q00" & cod_ons_3 <= "Q99" ~ "Congenital malformations, deformations and chromosomal abnormalities"
                                 , cod_ons_3 >= "V01" & cod_ons_3 <= "V89" ~ "Land transport accidents"
                                 , cod_ons_3 >= "W00" & cod_ons_3 <= "W19" ~ "Accidental falls"
                                 , cod_ons_3 >= "W32" & cod_ons_3 <= "W34" ~ "Non-intentional firearm discharge"
                                 , cod_ons_3 >= "W65" & cod_ons_3 <= "W74" ~ "Accidental drowning and submersion"
                                 , cod_ons_3 >= "W75" & cod_ons_3 <= "W84" ~ "Accidental threats to breathing"
                                 , cod_ons_3 >= "X40" & cod_ons_3 <= "X49" ~ "Accidental poisoning"
                                 , (cod_ons_3 >= "X60" & cod_ons_3 <= "X84") | (cod_ons_3 >= "Y10" & cod_ons_3 <= "Y34") ~ "Suicide and injury/poisoning of undetermined intent"
                                 , cod_ons_4 == "U509" | (cod_ons_3 >= "X85" & cod_ons_3 <= "Y09") | cod_ons_4 == "Y871" ~ "Homicide and probable suicide"
                                 , cod_ons_3 >= "R00" & cod_ons_3 <= "R99" ~ "Symptoms, signs and ill-defined conditions"
                                 , cod_ons_4 %in% c("U071","U072", "U109") ~ "COVID-19"
                                 , TRUE ~ "All other causes")) %>%
  group_by(study_quarter, pod_ons_new, lcod_ons_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod_leading_cod, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_leading_cod.csv"))

# Sex

deaths_quarter_pod_sex <- df_input %>%
  group_by(study_quarter, pod_ons_new, sex) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_sex, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_sex.csv"))

# Age group

deaths_quarter_pod_agegrp <- df_input %>%
  group_by(study_quarter, pod_ons_new, agegrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_agegrp, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_agegrp.csv"))

# Ethnicity

deaths_quarter_pod_ethnicity <- df_input %>%
  group_by(study_quarter, pod_ons_new, ethnicity) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_ethnicity, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_ethnicity.csv"))

# Ethnicity GP

deaths_quarter_pod_ethnicity_gp <- df_input %>%
  group_by(study_quarter, pod_ons_new, ethnicity_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_ethnicity_gp, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_ethnicity_gp.csv"))

# Ethnicity SUS

deaths_quarter_pod_ethnicity_sus <- df_input %>%
  group_by(study_quarter, pod_ons_new, ethnicity_sus) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_ethnicity_sus, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_ethnicity_sus.csv"))

#  Long term conditions

deaths_quarter_pod_ltc <- df_input %>%
  group_by(study_quarter, pod_ons_new, ltcgrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_ltc, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_ltc.csv"))

#  Palliative care

deaths_quarter_pod_palcare <- df_input %>%
  group_by(study_quarter, pod_ons_new, palcare) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_palcare, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_palcare.csv"))

#  No palliative care

deaths_quarter_pod_nopalcare <- df_input %>%
  group_by(study_quarter, pod_ons_new, nopalcare) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_nopalcare, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_nopalcare.csv"))

#  Care home

deaths_quarter_pod_carehome <- df_input %>%
  group_by(study_quarter, pod_ons_new, carehome) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_carehome, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_carehome.csv"))


# Region

deaths_quarter_pod_region <- df_input %>%
  group_by(study_quarter, pod_ons_new, region) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_region, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_region.csv"))

# Deprivation quintile

deaths_quarter_pod_imd <- df_input %>%
  group_by(study_quarter, pod_ons_new, imd_quintile) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod_imd, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_imd.csv"))

# Local authority deprivation quintile

deaths_quarter_pod_imd_la <- df_input %>%
  group_by(study_quarter, pod_ons_new, imd_quintile_la) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod_imd_la, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_imd_la.csv"))

# Rural urban

deaths_quarter_pod_rural_urban <- df_input %>%
  group_by(study_quarter, pod_ons_new, rural_urban) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_rural_urban, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_rural_urban.csv"))

# GP region

deaths_quarter_pod_region_gp <- df_input %>%
  group_by(study_quarter, pod_ons_new, region_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_region_gp, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_region_gp.csv"))

# Local authority deprivation quintile GP

deaths_quarter_pod_imd_la_gp <- df_input %>%
  group_by(study_quarter, pod_ons_new, imd_quintile_la_gp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod_imd_la_gp, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_imd_la_gp.csv"))

################################################################################

########## Descriptive table of study quarters by place of death ##########

quarter_pod_summary_table <- deaths_quarter_pod  %>% 
  mutate(variable = "n") %>% 
  bind_rows(deaths_quarter_pod_sex %>% 
              mutate(variable = "Sex"
                     , category = sex) %>%
              bind_rows(deaths_quarter_pod_agegrp %>%  
                          mutate(variable = "Age group"
                                 , category = agegrp)) %>% 
              bind_rows(deaths_quarter_pod_ethnicity %>%  
                          mutate(variable = "Ethnicity"
                                 , category = ethnicity)) %>% 
              bind_rows(deaths_quarter_pod_cod %>%  
                          mutate(variable = "Cause of death"
                                 , category = codgrp)) %>% 
              bind_rows(deaths_quarter_pod_ltc %>%  
                          mutate(variable = "Long term conditions"
                                 , category = ltcgrp)) %>%
              bind_rows(deaths_quarter_pod_palcare %>%  
                          mutate(variable = "Palliative care"
                                 , category = case_when(palcare == TRUE ~ "Yes"
                                                        , TRUE ~ "No"))) %>%
              bind_rows(deaths_quarter_pod_nopalcare %>%  
                          mutate(variable = "No palliative care"
                                 , category = case_when(nopalcare == TRUE ~ "Yes"
                                                        , TRUE ~ "No"))) %>%
              bind_rows(deaths_quarter_pod_carehome %>%  
                          mutate(variable = "Care home"
                                 , category = carehome)) %>%
              bind_rows(deaths_quarter_pod_region %>%  
                          mutate(variable = "Region"
                                 , category = region)) %>%
              bind_rows(deaths_quarter_pod_imd %>%  
                          mutate(variable = "IMD quintile"
                                 , category = as.character(imd_quintile))) %>% 
              bind_rows(deaths_quarter_pod_imd_la %>%  
                          mutate(variable = "LA IMD quintile"
                                 , category = as.character(imd_quintile_la))) %>% 
              bind_rows(deaths_quarter_pod_rural_urban %>%  
                          mutate(variable = "Rural urban"
                                 , category = rural_urban)) %>% 
              mutate(percent_study_quarter_1 = round(proportion_study_quarter_1 * 100, 1)
                     , percent_study_quarter_2 = round(proportion_study_quarter_2 * 100, 1)
                     , percent_study_quarter_3 = round(proportion_study_quarter_3 * 100, 1)
                     , percent_study_quarter_4 = round(proportion_study_quarter_4 * 100, 1)
                     , percent_study_quarter_5 = round(proportion_study_quarter_5 * 100, 1)
                     , percent_study_quarter_6 = round(proportion_study_quarter_6 * 100, 1)
                     , percent_study_quarter_7 = round(proportion_study_quarter_7 * 100, 1)
                     , percent_study_quarter_8 = round(proportion_study_quarter_8 * 100, 1))) %>% 
  select(pod_ons_new, variable, category, starts_with("deaths_study_quarter_"), starts_with("percent_study_quarter_")) %>% 
  arrange(pod_ons_new, factor(variable, levels = c("n", "Sex", "Age group", "Ethnicity", "Cause of death", "Long term conditions", "Palliative care"
                                                   , "No palliative care", "Care home", "Region", "IMD quintile", "LA IMD quintile", "Rural urban")), category)

write_csv(quarter_pod_summary_table, here::here("output", "describe_cohorts", "quarter_pod_summary_table.csv"))

################################################################################

########## Deaths by region and ethnicity  ##########

# Check for how representative TPP sample is

deaths_region_ethnicity <- df_input %>%
  group_by(region, ethnicity) %>%
  summarise(deaths = n()) %>%
  group_by(region) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total)

write_csv(deaths_region_ethnicity, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_region_ethnicity.csv"))

deaths_region_ethnicity_gp <- df_input %>%
  group_by(region, ethnicity_gp) %>%
  summarise(deaths = n()) %>%
  group_by(region) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total)

write_csv(deaths_region_ethnicity_gp, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_region_ethnicity_gp.csv"))

deaths_region_ethnicity_sus <- df_input %>%
  group_by(region, ethnicity_sus) %>%
  summarise(deaths = n()) %>%
  group_by(region) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total)

write_csv(deaths_region_ethnicity_sus, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_region_ethnicity_sus.csv"))

deaths_region_gp_ethnicity <- df_input %>%
  group_by(region_gp, ethnicity) %>%
  summarise(deaths = n()) %>%
  group_by(region_gp) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total)

write_csv(deaths_region_gp_ethnicity, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_region_gp_ethnicity.csv"))

deaths_region_gp_ethnicity_gp <- df_input %>%
  group_by(region_gp, ethnicity_gp) %>%
  summarise(deaths = n()) %>%
  group_by(region_gp) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total)

write_csv(deaths_region_gp_ethnicity_gp, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_region_gp_ethnicity_gp.csv"))

deaths_region_gp_ethnicity_sus <- df_input %>%
  group_by(region_gp, ethnicity_sus) %>%
  summarise(deaths = n()) %>%
  group_by(region_gp) %>%
  mutate(deaths = plyr::round_any(deaths, 10)
         , total = sum(deaths)
         , proportion = deaths / total)

write_csv(deaths_region_gp_ethnicity_sus, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_region_gp_ethnicity_sus.csv"))

################################################################################

########## Crosstabs of key characteristics  ##########

# sex, agegrp, ethnicity, imd_quintile, codgrp

deaths_characteristic_crosstabs <- df_input %>%
  filter(!is.na(study_cohort)) %>%
  mutate(agegrp = case_when(age >= 0 & age <= 29 ~ "00-29"
                            , age >= 30 & age <= 39 ~ "30-39"
                            , age >= 40 & age <= 49 ~ "40-49"
                            , age >= 50 & age <= 59 ~ "50-59"
                            , age >= 60 & age <= 69 ~ "60-69"
                            , age >= 70 & age <= 79 ~ "70-79"
                            , age >= 80 & age <= 89 ~ "80-89"
                            , age >= 90 ~ "90+"
                            , TRUE ~ NA_character_)) %>%
  group_by(cohort, sex, agegrp) %>%
  summarise(deaths = n()) %>%
  mutate(characteristic_1 = "sex"
         , characteristic_2 = "agegrp") %>%
  rename(category_1 = sex
         , category_2 = agegrp) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort)) %>%
              group_by(cohort, sex, ethnicity) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "sex"
                     , characteristic_2 = "ethnicity") %>%
              rename(category_1 = sex
                     , category_2 = ethnicity)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort)) %>%
              group_by(cohort, sex, imd_quintile) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "sex"
                     , characteristic_2 = "imd_quintile") %>%
              rename(category_1 = sex
                     , category_2 = imd_quintile)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort)) %>%
              group_by(cohort, sex, codgrp) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "sex"
                     , characteristic_2 = "codgrp") %>%
              rename(category_1 = sex
                     , category_2 = codgrp)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort)) %>%
              mutate(agegrp = case_when(age >= 0 & age <= 29 ~ "00-29"
                                        , age >= 30 & age <= 39 ~ "30-39"
                                        , age >= 40 & age <= 49 ~ "40-49"
                                        , age >= 50 & age <= 59 ~ "50-59"
                                        , age >= 60 & age <= 69 ~ "60-69"
                                        , age >= 70 & age <= 79 ~ "70-79"
                                        , age >= 80 & age <= 89 ~ "80-89"
                                        , age >= 90 ~ "90+"
                                        , TRUE ~ NA_character_)) %>%
              group_by(cohort, agegrp, ethnicity) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "agegrp"
                     , characteristic_2 = "ethnicity") %>%
              rename(category_1 = agegrp
                     , category_2 = ethnicity)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort)) %>%
              mutate(agegrp = case_when(age >= 0 & age <= 29 ~ "00-29"
                                        , age >= 30 & age <= 39 ~ "30-39"
                                        , age >= 40 & age <= 49 ~ "40-49"
                                        , age >= 50 & age <= 59 ~ "50-59"
                                        , age >= 60 & age <= 69 ~ "60-69"
                                        , age >= 70 & age <= 79 ~ "70-79"
                                        , age >= 80 & age <= 89 ~ "80-89"
                                        , age >= 90 ~ "90+"
                                        , TRUE ~ NA_character_)) %>%
              group_by(cohort, agegrp, imd_quintile) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "agegrp"
                     , characteristic_2 = "imd_quintile") %>%
              rename(category_1 = agegrp
                     , category_2 = imd_quintile)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort)) %>%
              mutate(agegrp = case_when(age >= 0 & age <= 29 ~ "00-29"
                                        , age >= 30 & age <= 39 ~ "30-39"
                                        , age >= 40 & age <= 49 ~ "40-49"
                                        , age >= 50 & age <= 59 ~ "50-59"
                                        , age >= 60 & age <= 69 ~ "60-69"
                                        , age >= 70 & age <= 79 ~ "70-79"
                                        , age >= 80 & age <= 89 ~ "80-89"
                                        , age >= 90 ~ "90+"
                                        , TRUE ~ NA_character_)) %>%
              group_by(cohort, agegrp, codgrp) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "agegrp"
                     , characteristic_2 = "codgrp") %>%
              rename(category_1 = agegrp
                     , category_2 = codgrp)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort)) %>%
              group_by(cohort, ethnicity, imd_quintile) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "ethnicity"
                     , characteristic_2 = "imd_quintile") %>%
              rename(category_1 = ethnicity
                     , category_2 = imd_quintile)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort)) %>%
              group_by(cohort, ethnicity, codgrp) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "ethnicity"
                     , characteristic_2 = "codgrp") %>%
              rename(category_1 = ethnicity
                     , category_2 = codgrp)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort)) %>%
              group_by(cohort, imd_quintile, codgrp) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "imd_quintile"
                     , characteristic_2 = "codgrp") %>%
              rename(category_1 = imd_quintile
                     , category_2 = codgrp)) %>%
  mutate(deaths = plyr::round_any(deaths, 10)) %>%
  select(cohort, characteristic_1, category_1, characteristic_2, category_2, deaths)

write_csv(deaths_characteristic_crosstabs, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_characteristic_crosstabs.csv"))

################################################################################

########## Crosstabs of key characteristics for home deaths  ##########

# sex, agegrp, ethnicity, imd_quintile, codgrp

categories_1 <- df_input %>%
  distinct(sex) %>%
  rename(category_1 = sex) %>%
  mutate(characteristic_1 = "sex") %>%
  bind_rows(df_input  %>%
              mutate(agegrp = case_when(age >= 0 & age <= 29 ~ "00-29"
                                        , age >= 30 & age <= 39 ~ "30-39"
                                        , age >= 40 & age <= 49 ~ "40-49"
                                        , age >= 50 & age <= 59 ~ "50-59"
                                        , age >= 60 & age <= 69 ~ "60-69"
                                        , age >= 70 & age <= 79 ~ "70-79"
                                        , age >= 80 & age <= 89 ~ "80-89"
                                        , age >= 90 ~ "90+"
                                        , TRUE ~ NA_character_)) %>%
              distinct(agegrp) %>%
              rename(category_1 = agegrp) %>%
              mutate(characteristic_1 = "agegrp")) %>%
  bind_rows(df_input %>%
              distinct(ethnicity)  %>%
              rename(category_1 = ethnicity) %>%
              mutate(characteristic_1 = "ethnicity")) %>%
  bind_rows(df_input %>%
              distinct(imd_quintile)  %>%
              rename(category_1 = imd_quintile) %>%
              mutate(characteristic_1 = "imd_quintile")) %>%
  bind_rows(df_input %>%
              distinct(codgrp)  %>%
              rename(category_1 = codgrp) %>%
              mutate(characteristic_1 = "codgrp")) 

`%notin%` <- Negate(`%in%`)

categories_2 <- merge(categories_1 %>%
  filter(characteristic_1 == "sex"), categories_1 %>%
              filter(characteristic_1 != "sex") %>%
    rename(characteristic_2 = characteristic_1
           , category_2 = category_1), all = TRUE) %>%
  bind_rows(merge(categories_1 %>%
                    filter(characteristic_1 == "agegrp"), categories_1 %>%
                    filter(characteristic_1 %notin% c("sex", "agegrp")) %>%
                    rename(characteristic_2 = characteristic_1
                           , category_2 = category_1), all = TRUE)) %>%
  bind_rows(merge(categories_1 %>%
                    filter(characteristic_1 == "ethnicity"), categories_1 %>%
                    filter(characteristic_1 %notin% c("sex", "agegrp", "ethnicity")) %>%
                    rename(characteristic_2 = characteristic_1
                           , category_2 = category_1), all = TRUE)) %>%
  bind_rows(merge(categories_1 %>%
                    filter(characteristic_1 == "imd_quintile"), categories_1 %>%
                    filter(characteristic_1 %notin% c("sex", "agegrp", "ethnicity", "imd_quintile")) %>%
                    rename(characteristic_2 = characteristic_1
                           , category_2 = category_1), all = TRUE)) 

home_deaths_characteristic_crosstabs <- categories_2 %>%
  mutate(cohort = 0) %>%
  bind_rows(categories_2 %>%
              mutate(cohort = 1)) %>%
  left_join(df_input %>%
  filter(!is.na(study_cohort) & pod_ons_new == "Home") %>%
  mutate(agegrp = case_when(age >= 0 & age <= 29 ~ "00-29"
                            , age >= 30 & age <= 39 ~ "30-39"
                            , age >= 40 & age <= 49 ~ "40-49"
                            , age >= 50 & age <= 59 ~ "50-59"
                            , age >= 60 & age <= 69 ~ "60-69"
                            , age >= 70 & age <= 79 ~ "70-79"
                            , age >= 80 & age <= 89 ~ "80-89"
                            , age >= 90 ~ "90+"
                            , TRUE ~ NA_character_)) %>%
  group_by(cohort, sex, agegrp) %>%
  summarise(deaths = n()) %>%
  mutate(characteristic_1 = "sex"
         , characteristic_2 = "agegrp") %>%
  rename(category_1 = sex
         , category_2 = agegrp) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort) & pod_ons_new == "Home") %>%
              group_by(cohort, sex, ethnicity) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "sex"
                     , characteristic_2 = "ethnicity") %>%
              rename(category_1 = sex
                     , category_2 = ethnicity)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort) & pod_ons_new == "Home") %>%
              group_by(cohort, sex, imd_quintile) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "sex"
                     , characteristic_2 = "imd_quintile") %>%
              rename(category_1 = sex
                     , category_2 = imd_quintile)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort) & pod_ons_new == "Home") %>%
              group_by(cohort, sex, codgrp) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "sex"
                     , characteristic_2 = "codgrp") %>%
              rename(category_1 = sex
                     , category_2 = codgrp)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort) & pod_ons_new == "Home") %>%
              mutate(agegrp = case_when(age >= 0 & age <= 29 ~ "00-29"
                                        , age >= 30 & age <= 39 ~ "30-39"
                                        , age >= 40 & age <= 49 ~ "40-49"
                                        , age >= 50 & age <= 59 ~ "50-59"
                                        , age >= 60 & age <= 69 ~ "60-69"
                                        , age >= 70 & age <= 79 ~ "70-79"
                                        , age >= 80 & age <= 89 ~ "80-89"
                                        , age >= 90 ~ "90+"
                                        , TRUE ~ NA_character_)) %>%
              group_by(cohort, agegrp, ethnicity) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "agegrp"
                     , characteristic_2 = "ethnicity") %>%
              rename(category_1 = agegrp
                     , category_2 = ethnicity)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort) & pod_ons_new == "Home") %>%
              mutate(agegrp = case_when(age >= 0 & age <= 29 ~ "00-29"
                                        , age >= 30 & age <= 39 ~ "30-39"
                                        , age >= 40 & age <= 49 ~ "40-49"
                                        , age >= 50 & age <= 59 ~ "50-59"
                                        , age >= 60 & age <= 69 ~ "60-69"
                                        , age >= 70 & age <= 79 ~ "70-79"
                                        , age >= 80 & age <= 89 ~ "80-89"
                                        , age >= 90 ~ "90+"
                                        , TRUE ~ NA_character_)) %>%
              group_by(cohort, agegrp, imd_quintile) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "agegrp"
                     , characteristic_2 = "imd_quintile") %>%
              rename(category_1 = agegrp
                     , category_2 = imd_quintile)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort) & pod_ons_new == "Home") %>%
              mutate(agegrp = case_when(age >= 0 & age <= 29 ~ "00-29"
                                        , age >= 30 & age <= 39 ~ "30-39"
                                        , age >= 40 & age <= 49 ~ "40-49"
                                        , age >= 50 & age <= 59 ~ "50-59"
                                        , age >= 60 & age <= 69 ~ "60-69"
                                        , age >= 70 & age <= 79 ~ "70-79"
                                        , age >= 80 & age <= 89 ~ "80-89"
                                        , age >= 90 ~ "90+"
                                        , TRUE ~ NA_character_)) %>%
              group_by(cohort, agegrp, codgrp) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "agegrp"
                     , characteristic_2 = "codgrp") %>%
              rename(category_1 = agegrp
                     , category_2 = codgrp)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort) & pod_ons_new == "Home") %>%
              group_by(cohort, ethnicity, imd_quintile) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "ethnicity"
                     , characteristic_2 = "imd_quintile") %>%
              rename(category_1 = ethnicity
                     , category_2 = imd_quintile)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort) & pod_ons_new == "Home") %>%
              group_by(cohort, ethnicity, codgrp) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "ethnicity"
                     , characteristic_2 = "codgrp") %>%
              rename(category_1 = ethnicity
                     , category_2 = codgrp)) %>%
  bind_rows(df_input %>%
              filter(!is.na(study_cohort) & pod_ons_new == "Home") %>%
              group_by(cohort, imd_quintile, codgrp) %>%
              summarise(deaths = n()) %>%
              mutate(characteristic_1 = "imd_quintile"
                     , characteristic_2 = "codgrp") %>%
              rename(category_1 = imd_quintile
                     , category_2 = codgrp)) %>%
  mutate(deaths = plyr::round_any(deaths, 10)) %>%
  select(cohort, characteristic_1, category_1, characteristic_2, category_2, deaths)
  , by = c("cohort", "characteristic_1", "category_1", "characteristic_2", "category_2")) %>%
  mutate(deaths = case_when(is.na(deaths) ~ 0
                            , TRUE ~ deaths))

write_csv(home_deaths_characteristic_crosstabs, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_characteristic_crosstabs_home.csv"))

################################################################################
