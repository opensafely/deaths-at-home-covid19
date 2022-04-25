################################################################################

########## DESCRIBE COHORTS ##########

################################################################################

# Number of deaths pre-pandemic and by pandemic phase - plots by quarter?
# Number of deaths by place - breakdown pre pandemic, quarter
# Assess completeness of cohorts relative to deaths reported by ONS
# Look at characteristics of people who died by place of death - ratios deaths pre pandemic to pandemic by characteristic

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
# Create cohort flag
# Death quarter variable starting in March so it is quarters of the cohort period rather than calendar or fiscal quarters
# Join on region and LA imd quintile

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
         , cod_ons_4 = str_sub(cod_ons, 1, 5)) %>%
  left_join(read_csv(here::here("docs", "lookups", "msoa_lad_rgn_2020.csv"))
            , by = c("msoa" = "msoa11cd")) %>%
  left_join(read_csv(here::here("docs", "lookups", "lad_imd_2019.csv"))
            , by = "lad20cd") %>%
  rename(imd_quintile_la = imd19_quintile) %>%
  mutate(imd_quintile_la = case_when(is.na(imd_quintile_la) ~ 0
                                     , TRUE ~ imd_quintile_la))

################################################################################

########## Basic death counts ##########

# Number of deaths by cohort

deaths_cohort <- df_input %>%
  group_by(cohort) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5))

write_csv(deaths_cohort, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_cohort.csv"))

# Number of deaths by quarter
deaths_quarter <- df_input %>%
  group_by(study_quarter) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5))

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

# Number of deaths by cohort and place of death

deaths_cohort_pod <- df_input %>%
  group_by(cohort, pod_ons) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total)

write_csv(deaths_cohort_pod, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_cohort_pod.csv"))

# Number of deaths by quarter and place of death

deaths_quarter_pod1 <- df_input %>%
  group_by(study_quarter, pod_ons) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total)

write_csv(deaths_quarter_pod1, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_quarter_pod.csv"))

# Number of deaths by cohort, place of death and cause of death
# Help to decide which cause of death groupings to use

deaths_cohort_pod_cod <- df_input %>%
  mutate(cod_ons_grp = case_when(cod_ons_3 >= "A00" & cod_ons_3 <= "A09" ~ "Intestinal infectious diseases"
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
  group_by(cohort, pod_ons, cod_ons_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, total, proportion))

write_csv(deaths_cohort_pod_cod, here::here("output", "describe_cohorts", "overall_death_counts", "deaths_cohort_pod_cod.csv"))

################################################################################

########## Plot death counts by place ##########

# Plot of number of deaths by place and cohort

plot_deaths_pod_cohort <- ggplot(deaths_cohort_pod) + 
  geom_bar(aes(x = reorder(pod_ons, deaths), y = deaths, fill = factor(cohort, levels = c("1", "0"))), stat = "identity", position = "dodge", width = 0.6) +
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
  geom_bar(aes(x = reorder(pod_ons, proportion), y = proportion, fill = factor(cohort, levels = c("1", "0"))), stat = "identity", position = "dodge", width = 0.6) +
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

########## Death counts by quarter and characteristics ##########

# Place of death

deaths_quarter_pod <- df_input %>%
  group_by(study_quarter, pod_ons) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod.csv"))

# Cause of death

deaths_quarter_cod <- df_input %>%
  mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                 , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                 , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                 , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                 , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                 , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                 , TRUE ~ "All other causes")) %>%
  group_by(study_quarter, cod_ons_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_cod, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_cod.csv"))

# Leading causes of death

deaths_quarter_leading_cod <- df_input %>%
  mutate(cod_ons_grp = case_when(cod_ons_3 >= "A00" & cod_ons_3 <= "A09" ~ "Intestinal infectious diseases"
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
  group_by(study_quarter, cod_ons_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_leading_cod, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_leading_cod.csv"))

# Sex

deaths_quarter_sex <- df_input %>%
  group_by(study_quarter, sex) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_sex, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_sex.csv"))

# Age group

deaths_quarter_agegrp <- df_input %>%
  mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "<10"
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
  group_by(study_quarter, agegrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_agegrp, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_agegrp.csv"))

# Ethnicity

deaths_quarter_ethnicity <- df_input %>%
  group_by(study_quarter, ethnicity) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_ethnicity, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_ethnicity.csv"))

#  Long term conditions

deaths_quarter_ltc <- df_input %>%
  mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
         , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                               , ltc_count >= 5 ~ "5+"
                               , TRUE ~ NA_character_)) %>%
  group_by(study_quarter, ltc_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_ltc, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_ltc.csv"))

#  Palliative care

deaths_quarter_palcare <- df_input %>%
  group_by(study_quarter, ltc_palcare1) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))
write_csv(deaths_quarter_palcare, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_palcare.csv"))

# MSOA

deaths_quarter_msoa <- df_input %>%
  mutate(msoa_present = case_when(is.na(msoa) ~ "No"
                              , TRUE ~ "Yes")) %>%
  group_by(study_quarter, msoa_present) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_msoa, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_msoa.csv"))

# Region

deaths_quarter_region <- df_input %>%
  group_by(study_quarter, rgn20cd) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_region, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_region.csv"))

# Deprivation quintile

deaths_quarter_imd <- df_input %>%
  group_by(study_quarter, imd_quintile) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_imd, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_imd.csv"))

# Local authority deprivation quintile

deaths_quarter_imd_la <- df_input %>%
  group_by(study_quarter, imd_quintile_la) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_imd_la, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_imd_la.csv"))

# Household size

deaths_quarter_alone <- df_input %>%
  mutate(lives_alone = case_when(hhold_size == 0 ~ NA_real_
                                 , hhold_size == 1 ~ 1
                                 , TRUE ~ 0)) %>%
  group_by(study_quarter, lives_alone) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_alone, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_alone.csv"))

################################################################################

########## Descriptive table of quarters ##########

quarter_summary_table <- deaths_quarter %>% 
  pivot_wider(names_from = study_quarter, names_prefix = "study_quarter_", values_from = deaths) %>% 
  mutate(variable = "n") %>% 
  bind_rows(deaths_quarter_sex %>% 
              mutate(variable = "Sex"
                     , category = sex) %>%
              select(variable, category, starts_with("proportion_study_quarter_")) %>% 
              bind_rows(deaths_quarter_agegrp %>%  
                          mutate(variable = "Age group"
                                 , category = agegrp) %>%
                          select(variable, category, starts_with("proportion_study_quarter_"))) %>% 
              bind_rows(deaths_quarter_ethnicity %>%  
                          mutate(variable = "Ethnicity"
                                 , category = ethnicity) %>%
                          select(variable, category, starts_with("proportion_study_quarter_"))) %>% 
              bind_rows(deaths_quarter_pod %>%  
                          mutate(variable = "Place of death"
                                 , category = pod_ons) %>%
                          select(variable, category, starts_with("proportion_study_quarter_"))) %>% 
              bind_rows(deaths_quarter_cod %>%  
                          mutate(variable = "Cause of death"
                                 , category = cod_ons_grp) %>%
                          select(variable, category, starts_with("proportion_study_quarter_"))) %>% 
              bind_rows(deaths_quarter_ltc %>%  
                          mutate(variable = "Long term conditions"
                                 , category = ltc_grp) %>%
                          select(variable, category, starts_with("proportion_study_quarter_"))) %>%
              bind_rows(deaths_quarter_palcare %>%  
                          mutate(variable = "Palliative care"
                                 , category = case_when(ltc_palcare1 == TRUE ~ "Yes"
                                                        , TRUE ~ "No")) %>%
                          select(variable, category, starts_with("proportion_study_quarter_"))) %>%
              mutate(study_quarter_1 = round(proportion_study_quarter_1 * 100, 1)
                     , study_quarter_2 = round(proportion_study_quarter_2 * 100, 1)
                     , study_quarter_3 = round(proportion_study_quarter_3 * 100, 1)
                     , study_quarter_4 = round(proportion_study_quarter_4 * 100, 1)
                     , study_quarter_5 = round(proportion_study_quarter_5 * 100, 1)
                     , study_quarter_6 = round(proportion_study_quarter_6 * 100, 1)
                     , study_quarter_7 = round(proportion_study_quarter_7 * 100, 1)
                     , study_quarter_8 = round(proportion_study_quarter_8 * 100, 1))) %>% 
  select(variable, category, starts_with("study_quarter_"))

write_csv(quarter_summary_table, here::here("output", "describe_cohorts", "quarter_summary_table.csv"))

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
  mutate(deaths = plyr::round_any(deaths, 5)) %>%
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
  group_by(study_month, rgn20cd, study_quarter) %>%
  summarise(deaths = n()) %>%
  left_join(read_csv(here::here("docs", "ons_comparison_data", "region_onsmortality.csv"))
            , by = c("study_month" = "period", "rgn20cd" = "region")) %>%
  group_by(study_quarter, rgn20cd) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)
            , ons_deaths = sum(ons_deaths, na.rm = TRUE)) %>%
  mutate(deaths = plyr::round_any(deaths, 5)) %>%
  mutate(proportion = deaths / ons_deaths)

write_csv(deaths_ons_quarter_region, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_quarter_region.csv"))

plot_deaths_ons_quarter_region <- ggplot(deaths_ons_quarter_region %>% filter(str_detect(rgn20cd, "^E12")), aes(x = study_quarter, y = proportion, colour = rgn20cd)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  labs(x = "Study quarter", y = "Percent of ONS deaths") +
  scale_colour_manual(values = c("#9F67FF", "#00C27A", "#FF6B57", "#4DCFF5", "#EABE17", "#0066F4", "#D3C4FC", "#8BF8BD", "#FFCFC9")) +
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
  mutate(deaths = plyr::round_any(deaths, 5)) %>%
  mutate(proportion = deaths / ons_deaths)

write_csv(deaths_ons_quarter_sex, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_quarter_sex.csv"))

plot_deaths_ons_quarter_sex <- ggplot(deaths_ons_quarter_sex %>% filter(sex %in% c("M", "F")), aes(x = study_quarter, y = proportion, colour = sex)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  labs(x = "Study quarter", y = "Percent of ONS deaths") +
  scale_colour_manual(values = c("#9F67FF", "#00C27A")) +
  scale_x_continuous(expand = c(0, 1), breaks = seq(1, 8, 1)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, 0.1)
                     , labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
  NT_style()

ggsave(plot = plot_deaths_ons_quarter_sex, filename ="deaths_ons_quarter_sex.png", path = here::here("output", "describe_cohorts", "ons_death_comparisons"), height = 10, width = 13.7, units = "cm", dpi = 600)

# Quarterly deaths (Mar 19 - Feb 21) by age group (<75, 75-79, 80-84, 85-89, 90+)  - Table 4, 8c

deaths_ons_quarter_agegrp <- df_input %>%
  filter(study_month >= as_date("2019-03-01") & study_month <= as_date("2021-02-01")) %>%
  mutate(agegrp = case_when(age >= 0 & age <= 74 ~ "<75"
                            , age >= 75 & age <= 79 ~ "75-79"
                            , age >= 80 & age <= 84 ~ "80-84"
                            , age >= 85 & age <= 89 ~ "85-89"
                            , age >= 90 ~ "90+"
                            , TRUE ~ NA_character_)) %>%
  group_by(study_month, agegrp, study_quarter) %>%
  summarise(deaths = n()) %>%
  left_join(read_csv(here::here("docs", "ons_comparison_data", "table4_8c_agegrp_onsmortality.csv"))
            , by = c("study_month" = "period", "agegrp"))  %>%
  group_by(study_quarter, agegrp) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)
            , ons_deaths = sum(ons_deaths, na.rm = TRUE)) %>%
  mutate(deaths = plyr::round_any(deaths, 5)) %>%
  mutate(proportion = deaths / ons_deaths)

write_csv(deaths_ons_quarter_agegrp, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_quarter_agegrp.csv"))

plot_deaths_ons_quarter_agegrp <- ggplot(deaths_ons_quarter_agegrp %>% filter(!is.na(agegrp)), aes(x = study_quarter, y = proportion, colour = agegrp)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  labs(x = "Study quarter", y = "Percent of ONS deaths") +
  scale_colour_manual(values = c("#9F67FF", "#00C27A", "#FF6B57", "#4DCFF5", "#EABE17")) +
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
deaths_ons_quarter_cod <- df_input %>%
  filter(study_month >= as_date("2019-03-01") & study_month <= as_date("2021-02-01")) %>%
  mutate(cod_ons_grp = case_when(cod_ons_3 >= "A00" & cod_ons_3 <= "A09" ~ "Intestinal infectious diseases"
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
  filter(cod_ons_grp != "All other causes") %>%
  group_by(study_month, cod_ons_grp, study_quarter) %>%
  summarise(deaths = n()) %>%
  mutate(cod_ons_grp = tolower(cod_ons_grp))  %>%
  left_join(read_csv(here::here("docs", "ons_comparison_data", "table11a_cod_onsmortality.csv"))
            , by = c("study_month" = "period", "cod_ons_grp" = "cause"))  %>%
  group_by(study_quarter, cod_ons_grp) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE)
            , ons_deaths = sum(ons_deaths, na.rm = TRUE)) %>%
  arrange(study_quarter, desc(deaths)) %>%
  mutate(rank = row_number()) %>%
  arrange(study_quarter, desc(ons_deaths)) %>%
  mutate(rank_ons = row_number()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)) %>%
  mutate(proportion = deaths / ons_deaths) %>%
  arrange(study_quarter, rank)

write_csv(deaths_ons_quarter_cod, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_quarter_cod.csv"))

# Monthly deaths (Jan 20 - Feb 21) by place of death - Table 14a

deaths_ons_month_pod <- df_input %>%
  filter(study_month >= as_date("2020-01-01") & study_month <= as_date("2021-02-01")) %>%
  group_by(study_month, pod_ons) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)) %>%
  left_join(read_csv(here::here("docs", "ons_comparison_data", "table14a_pod_onsmortality.csv"))
            , by = c("study_month" = "period", "pod_ons" = "place_of_death")) %>%
  mutate(proportion = deaths / ons_deaths)

write_csv(deaths_ons_month_pod, here::here("output", "describe_cohorts", "ons_death_comparisons", "deaths_ons_month_pod.csv"))

plot_deaths_ons_month_pod <- ggplot(deaths_ons_month_pod %>% filter(pod_ons %in% c("Hospital", "Care home", "Home", "Hospice", "Elsewhere", "Other communal establishment"))
                                    , aes(x = study_month, y = proportion, colour = pod_ons)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  labs(x = "Month", y = "Percent of ONS deaths") +
  scale_colour_manual(values = c("#9F67FF", "#00C27A", "#FF6B57", "#4DCFF5", "#EABE17", "#0066F4")) +
  scale_x_date(expand = c(0, 1), date_breaks = "month", date_labels = "%b %y") +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1), breaks = seq(0, 1, 0.1)
                     , labels = c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%", "80%", "90%", "100%")) +
  NT_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(plot = plot_deaths_ons_month_pod, filename ="deaths_ons_month_pod.png", path = here::here("output", "describe_cohorts", "ons_death_comparisons"), height = 10, width = 13.7, units = "cm", dpi = 600)

################################################################################

########## Ratios of deaths by characteristics ##########

# Ratio - place of death

death_ratio_pod <- df_input %>%
  group_by(cohort, pod_ons) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_pod, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_pod.csv"))

# Ratio - cause of death

death_ratio_cod <- df_input %>%
  mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                 , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                 , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                 , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                 , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                 , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                 , TRUE ~ "All other causes")) %>%
  group_by(cohort, cod_ons_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_cod, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_cod.csv"))

# Ratio - sex

death_ratio_sex <- df_input %>%
  group_by(cohort, sex) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_sex, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_sex.csv"))

#  Ratio - age group

death_ratio_agegrp <- df_input %>%
  mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "<10"
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
  group_by(cohort, agegrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_agegrp, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_agegrp.csv"))

#  Ratio - ethnicity

death_ratio_ethnicity <- df_input %>%
  group_by(cohort, ethnicity) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_ethnicity, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_ethnicity.csv"))

#  Ratio - long term conditions

death_ratio_ltc <- df_input %>%
  mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
         , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                               , ltc_count >= 5 ~ "5+"
                               , TRUE ~ NA_character_)) %>%
  group_by(cohort, ltc_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_ltc, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_ltc.csv"))

#  Ratio - palliative care

death_ratio_palcare <- df_input %>%
  group_by(cohort, ltc_palcare1) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_palcare, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_palcare.csv"))

# Ratio - Region

death_ratio_region <- df_input %>%
  group_by(cohort, rgn20cd) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_region, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_region.csv"))

# Ratio - Deprivation quintile

death_ratio_imd <- df_input %>%
  group_by(cohort, imd_quintile) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_imd, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_imd.csv"))

# Ratio - Local authority deprivation quintile

death_ratio_imd_la <- df_input %>%
  group_by(cohort, imd_quintile_la) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_imd_la, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_imd_la.csv"))

# Ratio - Household size

death_ratio_alone <- df_input %>%
  mutate(lives_alone = case_when(hhold_size == 0 ~ NA_real_
                                 , hhold_size == 1 ~ 1
                                 , TRUE ~ 0)) %>%
  group_by(cohort, lives_alone) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0)

write_csv(death_ratio_alone, here::here("output", "describe_cohorts", "death_ratios_cohort", "death_ratio_cohort_alone.csv"))

################################################################################

########## Descriptive table of cohorts ##########

cohorts_summary_table <- deaths_cohort %>% 
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = deaths) %>% 
  mutate(variable = "n") %>% 
  bind_rows(death_ratio_sex %>% 
  mutate(variable = "Sex"
         , category = sex) %>%
  select(variable, category, proportion_cohort_0, proportion_cohort_1) %>% 
  bind_rows(death_ratio_agegrp %>%  
              mutate(variable = "Age group"
                     , category = agegrp) %>%
              select(variable, category, proportion_cohort_0, proportion_cohort_1)) %>% 
  bind_rows(death_ratio_ethnicity %>%  
              mutate(variable = "Ethnicity"
                     , category = ethnicity) %>%
              select(variable, category, proportion_cohort_0, proportion_cohort_1)) %>% 
  bind_rows(death_ratio_pod %>%  
              mutate(variable = "Place of death"
                     , category = pod_ons) %>%
              select(variable, category, proportion_cohort_0, proportion_cohort_1)) %>% 
  bind_rows(death_ratio_cod %>%  
              mutate(variable = "Cause of death"
                     , category = cod_ons_grp) %>%
              select(variable, category, proportion_cohort_0, proportion_cohort_1)) %>% 
  bind_rows(death_ratio_ltc %>%  
              mutate(variable = "Long term conditions"
                     , category = ltc_grp) %>%
              select(variable, category, proportion_cohort_0, proportion_cohort_1)) %>%
    bind_rows(death_ratio_palcare %>%  
                mutate(variable = "Palliative care"
                       , category = case_when(ltc_palcare1 == TRUE ~ "Yes"
                                              , TRUE ~ "No")) %>%
                select(variable, category, proportion_cohort_0, proportion_cohort_1)) %>%
  mutate(cohort_0 = round(proportion_cohort_0 * 100, 1)
         , cohort_1 = round(proportion_cohort_1 * 100, 1))) %>% 
  select(variable, category, cohort_0, cohort_1)

write_csv(cohorts_summary_table, here::here("output", "describe_cohorts", "cohorts_summary_table.csv"))

################################################################################

########## Ratios of deaths by place of death for characteristics ##########

# Ratio - pod * cause of death

death_ratio_pod_cod <- df_input %>%
  mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                 , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                 , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                 , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                 , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                 , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                 , TRUE ~ "All other causes")) %>%
  group_by(cohort, pod_ons, cod_ons_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons, cod_ons_grp)

write_csv(death_ratio_pod_cod, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_cod.csv"))

#  Ratio - pod * sex

death_ratio_pod_sex <- df_input %>%
  group_by(cohort, pod_ons, sex) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons, sex)

write_csv(death_ratio_pod_sex, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_sex.csv"))

#  Ratio - pod * age group

death_ratio_pod_agegrp <- df_input %>%
  mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "<10"
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
  group_by(cohort, pod_ons, agegrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons, agegrp)

write_csv(death_ratio_pod_agegrp, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_agegrp.csv"))

#  Ratio - pod * ethnicity

death_ratio_pod_ethnicity <- df_input %>%
  group_by(cohort, pod_ons, ethnicity) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons, ethnicity)

write_csv(death_ratio_pod_ethnicity, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_ethnicity.csv"))

#  Ratio - pod * long term conditions

death_ratio_pod_ltc <- df_input %>%
  mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
         , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                               , ltc_count >= 5 ~ "5+"
                               , TRUE ~ NA_character_)) %>%
  group_by(cohort, pod_ons, ltc_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons, ltc_grp)

write_csv(death_ratio_pod_ltc, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_ltc.csv"))

#  Ratio - pod * palliative care

death_ratio_pod_palcare <- df_input %>%
  group_by(cohort, pod_ons, ltc_palcare1) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons, ltc_palcare1)

write_csv(death_ratio_pod_palcare, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_palcare.csv"))

# Ratio - pod * Region

death_ratio_pod_region <- df_input %>%
  group_by(cohort, pod_ons, rgn20cd) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons, rgn20cd)

write_csv(death_ratio_pod_region, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_region.csv"))

# Ratio - pod * Deprivation quintile

death_ratio_pod_imd <- df_input %>%
  group_by(cohort, pod_ons, imd_quintile) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons, imd_quintile)

write_csv(death_ratio_pod_imd, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_imd.csv"))

# Ratio - pod * Local authority deprivation quintile

death_ratio_pod_imd_la <- df_input %>%
  group_by(cohort, pod_ons, imd_quintile_la) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons, imd_quintile_la)

write_csv(death_ratio_pod_imd_la, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_imd_la.csv"))

# Ratio - pod * Household size

death_ratio_pod_alone <- df_input %>%
  mutate(lives_alone = case_when(hhold_size == 0 ~ NA_real_
                                 , hhold_size == 1 ~ 1
                                 , TRUE ~ 0)) %>%
  group_by(cohort, pod_ons, lives_alone) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = cohort, names_prefix = c("cohort_"), values_from = c(deaths, proportion)) %>%
  mutate(ratio_deaths = deaths_cohort_1 / deaths_cohort_0
         , ratio_proportion = proportion_cohort_1 / proportion_cohort_0) %>%
  arrange(pod_ons, lives_alone)

write_csv(death_ratio_pod_alone, here::here("output", "describe_cohorts", "death_ratios_pod_cohort", "death_ratio_pod_cohort_alone.csv"))

################################################################################

########## Descriptive table of cohorts by place of death ##########

cohorts_pod_summary_table <- deaths_cohort_pod %>% 
  select(cohort, pod_ons, deaths) %>% 
  pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = deaths) %>% 
  mutate(variable = "n") %>% 
  bind_rows(death_ratio_pod_sex %>% 
              mutate(variable = "Sex"
                     , category = sex) %>%
              select(pod_ons, variable, category, proportion_cohort_0, proportion_cohort_1) %>% 
              bind_rows(death_ratio_pod_agegrp %>%  
                          mutate(variable = "Age group"
                                 , category = agegrp) %>%
                          select(pod_ons, variable, category, proportion_cohort_0, proportion_cohort_1)) %>% 
              bind_rows(death_ratio_pod_ethnicity %>%  
                          mutate(variable = "Ethnicity"
                                 , category = ethnicity) %>%
                          select(pod_ons, variable, category, proportion_cohort_0, proportion_cohort_1)) %>% 
              bind_rows(death_ratio_pod_cod %>%  
                          mutate(variable = "Cause of death"
                                 , category = cod_ons_grp) %>%
                          select(pod_ons, variable, category, proportion_cohort_0, proportion_cohort_1)) %>% 
              bind_rows(death_ratio_pod_ltc %>%  
                          mutate(variable = "Long term conditions"
                                 , category = ltc_grp) %>%
                          select(pod_ons, variable, category, proportion_cohort_0, proportion_cohort_1)) %>%
              bind_rows(death_ratio_pod_palcare %>%  
                          mutate(variable = "Palliative care"
                                 , category = case_when(ltc_palcare1 == TRUE ~ "Yes"
                                                        , TRUE ~ "No")) %>%
                          select(pod_ons, variable, category, proportion_cohort_0, proportion_cohort_1)) %>%
              mutate(cohort_0 = round(proportion_cohort_0 * 100, 1)
                     , cohort_1 = round(proportion_cohort_1 * 100, 1))) %>% 
  select(pod_ons, variable, category, cohort_0, cohort_1) %>% 
  arrange(pod_ons, factor(variable, levels = c("n", "Sex", "Age group", "Ethnicity", "Cause of death", "Long term conditions", "Palliative care")), category)

write_csv(cohorts_pod_summary_table, here::here("output", "describe_cohorts", "cohorts_pod_summary_table.csv"))

################################################################################

########## Death counts by place of death, quarter and characteristics ##########

# Cause of death

deaths_quarter_pod_cod <- df_input %>%
  mutate(cod_ons_grp = case_when(cod_ons_4 %in% c("U071", "U072") ~ "Covid-19"
                                 , cod_ons_3 >= "J09" & cod_ons_3 <= "J18" ~ "Flu and pneumonia"
                                 , (cod_ons_3 >= "J00" & cod_ons_3 <= "J08") | (cod_ons_3 >= "J19" & cod_ons_3 <= "J99")  ~ "Other respiratory diseases"
                                 , cod_ons_3 %in% c("F01", "F03", "G30") ~ "Dementia and Alzheimer's disease"
                                 , cod_ons_3 >= "I00" & cod_ons_3 <= "I99" ~ "Circulatory diseases"
                                 , cod_ons_3 >= "C00" & cod_ons_3 <= "C99" ~ "Cancer"
                                 , TRUE ~ "All other causes")) %>%
  group_by(study_quarter, pod_ons, cod_ons_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod_cod, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_cod.csv"))

# Leading causes of death

deaths_quarter_pod_leading_cod <- df_input %>%
  mutate(cod_ons_grp = case_when(cod_ons_3 >= "A00" & cod_ons_3 <= "A09" ~ "Intestinal infectious diseases"
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
  group_by(study_quarter, pod_ons, cod_ons_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod_leading_cod, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_leading_cod.csv"))

# Sex

deaths_quarter_pod_sex <- df_input %>%
  group_by(study_quarter, pod_ons, sex) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_sex, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_sex.csv"))

# Age group

deaths_quarter_pod_agegrp <- df_input %>%
  mutate(agegrp = case_when(age >= 0 & age <= 10 ~ "<10"
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
  group_by(study_quarter, pod_ons, agegrp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_agegrp, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_agegrp.csv"))

# Ethnicity

deaths_quarter_pod_ethnicity <- df_input %>%
  group_by(study_quarter, pod_ons, ethnicity) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_ethnicity, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_ethnicity.csv"))

#  Long term conditions

deaths_quarter_pod_ltc <- df_input %>%
  mutate(ltc_count = df_input %>% select(starts_with("ltc_")) %>% rowSums()
         , ltc_grp = case_when(ltc_count < 5 ~ as.character(ltc_count)
                               , ltc_count >= 5 ~ "5+"
                               , TRUE ~ NA_character_)) %>%
  group_by(study_quarter, pod_ons, ltc_grp) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_ltc, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_ltc.csv"))

#  Palliative care

deaths_quarter_pod_palcare <- df_input %>%
  group_by(study_quarter, pod_ons, ltc_palcare1) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))
write_csv(deaths_quarter_pod_palcare, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_palcare.csv"))

# Region

deaths_quarter_pod_region <- df_input %>%
  group_by(study_quarter, pod_ons, rgn20cd) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion))

write_csv(deaths_quarter_pod_region, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_region.csv"))

# Deprivation quintile

deaths_quarter_pod_imd <- df_input %>%
  group_by(study_quarter, pod_ons, imd_quintile) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod_imd, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_imd.csv"))

# Local authority deprivation quintile

deaths_quarter_pod_imd_la <- df_input %>%
  group_by(study_quarter, pod_ons, imd_quintile_la) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod_imd_la, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_imd_la.csv"))

# Household size

deaths_quarter_pod_alone <- df_input %>%
  mutate(lives_alone = case_when(hhold_size == 0 ~ NA_real_
                                 , hhold_size == 1 ~ 1
                                 , TRUE ~ 0)) %>%
  group_by(study_quarter, pod_ons, lives_alone) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)
         , total = sum(deaths)
         , proportion = deaths / total) %>%
  select(-total) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths, proportion)) 

write_csv(deaths_quarter_pod_alone, here::here("output", "describe_cohorts", "quarter_death_counts", "deaths_quarter_pod_alone.csv"))

################################################################################

########## Descriptive table of study quarters by place of death ##########

quarter_pod_summary_table <- df_input %>%
  group_by(study_quarter, pod_ons) %>%
  summarise(deaths = n()) %>%
  mutate(deaths = plyr::round_any(deaths, 5)) %>%
  pivot_wider(names_from = study_quarter, names_prefix = c("study_quarter_"), values_from = c(deaths))  %>% 
  mutate(variable = "n") %>% 
  bind_rows(deaths_quarter_pod_sex %>% 
              mutate(variable = "Sex"
                     , category = sex) %>%
              select(pod_ons, variable, category, starts_with("proportion_study_quarter_")) %>% 
              bind_rows(deaths_quarter_pod_agegrp %>%  
                          mutate(variable = "Age group"
                                 , category = agegrp) %>%
                          select(pod_ons, variable, category, starts_with("proportion_study_quarter_"))) %>% 
              bind_rows(deaths_quarter_pod_ethnicity %>%  
                          mutate(variable = "Ethnicity"
                                 , category = ethnicity) %>%
                          select(pod_ons, variable, category, starts_with("proportion_study_quarter_"))) %>% 
              bind_rows(deaths_quarter_pod_cod %>%  
                          mutate(variable = "Cause of death"
                                 , category = cod_ons_grp) %>%
                          select(pod_ons, variable, category, starts_with("proportion_study_quarter_"))) %>% 
              bind_rows(deaths_quarter_pod_ltc %>%  
                          mutate(variable = "Long term conditions"
                                 , category = ltc_grp) %>%
                          select(pod_ons, variable, category, starts_with("proportion_study_quarter_"))) %>%
              bind_rows(deaths_quarter_pod_palcare %>%  
                          mutate(variable = "Palliative care"
                                 , category = case_when(ltc_palcare1 == TRUE ~ "Yes"
                                                        , TRUE ~ "No")) %>%
                          select(pod_ons, variable, category, starts_with("proportion_study_quarter_"))) %>%
              mutate(study_quarter_1 = round(proportion_study_quarter_1 * 100, 1)
                     , study_quarter_2 = round(proportion_study_quarter_2 * 100, 1)
                     , study_quarter_3 = round(proportion_study_quarter_3 * 100, 1)
                     , study_quarter_4 = round(proportion_study_quarter_4 * 100, 1)
                     , study_quarter_5 = round(proportion_study_quarter_5 * 100, 1)
                     , study_quarter_6 = round(proportion_study_quarter_6 * 100, 1)
                     , study_quarter_7 = round(proportion_study_quarter_7 * 100, 1)
                     , study_quarter_8 = round(proportion_study_quarter_8 * 100, 1))) %>% 
  select(pod_ons, variable, category, starts_with("study_quarter_")) %>% 
  arrange(pod_ons, factor(variable, levels = c("n", "Sex", "Age group", "Ethnicity", "Cause of death", "Long term conditions", "Palliative care")), category)

write_csv(quarter_pod_summary_table, here::here("output", "describe_cohorts", "quarter_pod_summary_table.csv"))

################################################################################
