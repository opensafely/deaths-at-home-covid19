################################################################################

########## DESCRIBE COHORTS ##########

################################################################################

# Number of deaths pre-pandemic and by pandemic phase - plots by quarter?
# Number of deaths by place - breakdown pre pandemic, pandemic phase, quarter
# Assess completeness of cohorts relative to all deaths
# Look at characteristics of people who died - ratios pre pandemic to pandemic phases

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

########## Basic death counts ##########

# Number of deaths by cohort

deaths_cohort <- df_input %>%
    group_by(cohort) %>%
    summarise(deaths = n())

write_csv(deaths_cohort, here::here("output", "deaths_cohort.csv"))

# Number of deaths by quarter
deaths_quarter <- df_input %>%
    group_by(study_quarter) %>%
    summarise(deaths = n())

write_csv(deaths_quarter, here::here("output", "deaths_quarter.csv"))

################################################################################

########## Plot basic death counts ##########

# Deaths by month

plot_dod_month <- ggplot(df_input, aes(x = floor_date(dod_ons, unit = "month"))) + 
  geom_bar(fill = "#9F67FF") +
  labs(x = "Month", y = "Number of deaths") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y", expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_text(colour = "#9AA0AA", size = 8, family = "sans", angle = 45, hjust=1),
    axis.text.y = element_text(colour = "#9AA0AA", size = 8, family = "sans"),
    axis.title.x = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold"),
    axis.title.y = element_text(margin = margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold", angle = 90),
    panel.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#9AA0AA"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit = "cm"), colour = "#271544", size = 10, face = "bold", hjust = 0),
    plot.title.position = "plot")

ggsave(plot = plot_dod_month, filename ="dod_month.png", path = here::here("output"))

################################################################################

########## Death counts by place ##########

# Number of deaths by cohort and place of death

deaths_cohort_pod <- df_input %>%
    group_by(cohort, pod_ons) %>%
    summarise(deaths = n()) %>%
    mutate(total = sum(deaths)
            , proportion = deaths / total)

write_csv(deaths_cohort_pod, here::here("output", "deaths_cohort_pod.csv"))

# Number of deaths by quarter and place of death

deaths_quarter_pod <- df_input %>%
    group_by(study_quarter, pod_ons) %>%
    summarise(deaths = n())

write_csv(deaths_quarter_pod, here::here("output", "deaths_quarter_pod.csv"))

################################################################################

########## Plot death counts by place ##########

# Plot of number of deaths by place and cohort

plot_pod_cohort <- ggplot(deaths_cohort_pod) + 
  geom_bar(aes(x = reorder(pod_ons, deaths), y = deaths, fill = factor(cohort, levels = c("1", "0"))), stat = "identity", position = "dodge", width = 0.6) +
  coord_flip() +
  labs(x = "Place of death", y = "Number of deaths") +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  scale_fill_manual(values = c("0" = "#00C27A", "1" = "#9F67FF"), labels = c("0" = "Pre-pandemic", "1" = "Pandemic")) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_text(colour = "#9AA0AA", size = 8, family = "sans", angle = 45, hjust=1),
    axis.text.y = element_text(colour = "#9AA0AA", size = 8, family = "sans", hjust = 1),
    axis.title.x = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold"),
    axis.title.y = element_text(margin = margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold", angle = 90),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
    legend.key = element_blank(),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
    panel.border = element_blank(),
    panel.grid.major.x = element_line(colour = "#9AA0AA", size = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit = "cm"), colour = "#271544", size = 10, face = "bold", hjust = 0),
    plot.title.position = "plot")

ggsave(plot = plot_pod_cohort, filename ="pod_cohort.png", path = here::here("output"))
    
# Plot of proportion of deaths by place and cohort

plot_pod_cohort_prop <- ggplot(deaths_cohort_pod) + 
  geom_bar(aes(x = reorder(pod_ons, proportion), y = proportion, fill = factor(cohort, levels = c("1", "0"))), stat = "identity", position = "dodge", width = 0.6) +
  coord_flip() +
  labs(x = "Place of death", y = "Proportion of deaths") +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_fill_manual(values = c("0" = "#00C27A", "1" = "#9F67FF"), labels = c("0" = "Pre-pandemic", "1" = "Pandemic")) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    axis.text.x = element_text(colour = "#9AA0AA", size = 8, family = "sans", angle = 45, hjust=1),
    axis.text.y = element_text(colour = "#9AA0AA", size = 8, family = "sans", hjust = 1),
    axis.title.x = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold"),
    axis.title.y = element_text(margin = margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold", angle = 90),
    legend.background = element_blank(),
    legend.box.background = element_blank(),
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
    legend.key = element_blank(),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
    legend.key.size = unit(0.4, "cm"),
    legend.position = "bottom",
    legend.spacing.x = unit(0.1, "cm"),
    legend.spacing.y = unit(0.1, "cm"),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
    panel.border = element_blank(),
    panel.grid.major.x = element_line(colour = "#9AA0AA", size = 0.3),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit = "cm"), colour = "#271544", size = 10, face = "bold", hjust = 0),
    plot.title.position = "plot")

ggsave(plot = plot_pod_cohort_prop, filename ="pod_cohort_prop.png", path = here::here("output"))

################################################################################

########## Create tables to compare to published ONS deaths ##########

# Monthly - sex (Mar 19 - Feb 21) Table 1, age group <75, 75-79, 80-84, 85-89, 90+ (Mar 19 - Feb 21) Table 4, 8c,  cod (Jul 20 - Feb 21) Table 11a, pod (Jan 20 - Feb 21) Table 14a

################################################################################

########## Ratios of deaths by place of death for characteristics ##########

# Ratio - pod

deaths_ratio_pod <- df_input %>%
    group_by(cohort, pod_ons) %>%
    summarise(deaths = n()) %>%
    pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = deaths) %>%
    mutate(ratio = cohort_1 / cohort_0)
    
write_csv(deaths_ratio_pod, here::here("output", "deaths_ratio_pod.csv"))

#  Ratio - pod * sex

deaths_ratio_pod_sex <- df_input %>%
    group_by(cohort, pod_ons, sex) %>%
    summarise(deaths = n()) %>%
    pivot_wider(names_from = cohort, names_prefix = "cohort_", values_from = deaths) %>%
    mutate(ratio = cohort_1 / cohort_0)

write_csv(deaths_ratio_pod_sex, here::here("output", "deaths_ratio_pod_sex.csv"))

################################################################################
