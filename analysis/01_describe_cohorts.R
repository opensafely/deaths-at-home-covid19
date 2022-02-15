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

########## Basic death counts ##########

# Number of deaths by cohort

deaths_cohort <- df_input %>%
    group_by(cohort) %>%
    summarise(deaths = n())

write_csv(deaths_cohort, filename ="deaths_cohort.csv", path = here::here("output"))

# Number of deaths by quarter
deaths_quarter <- df_input %>%
    group_by(dod_quarter) %>%
    summarise(deaths = n())

write_csv(deaths_quarter, filename ="deaths_quarter.csv", path = here::here("output"))

################################################################################

########## Plot basic death counts ##########

# Deaths by quarter

plot_dod_quarter <- ggplot(deaths_quarter, aes(x = dod_quarter, y = count)) + 
  geom_bar(stat = "identity", fill = "#9F67FF") +
  labs(x = "Death quarter", y = "Number of people") +
  #scale_x_date(date_breaks = "1 month", date_labels = "%b %y", limits = c(as.Date("2019-03-01"), as.Date("2021-02-28")), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
    panel.border = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#9AA0AA"),
    panel.grid.minor = element_blank(),

    plot.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit = "cm"), colour = "#271544", size = 10, face = "bold", hjust = 0),
    plot.title.position = "plot",

    axis.ticks = element_blank(),
    axis.text.x = element_text(colour = "#9AA0AA", size = 8, family = "sans", angle = 45, hjust=1),
    axis.text.y = element_text(colour = "#9AA0AA", size = 8, family = "sans"),
    axis.title.x = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold"),
    axis.title.y = element_text(margin = margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold", angle = 90)
)

ggsave(plot = plot_dod_quarter, filename ="dod_quarter.png", path = here::here("output"))

################################################################################

########## Death counts by place ##########

# Number of deaths by cohort and place of death

deaths_cohort_pod <- df_input %>%
    group_by(cohort, pod_ons) %>%
    summarise(deaths = n()) %>%
    mutate(total = sum(n)
            , proportion = deaths / total)

write_csv(deaths_cohort_pod, filename ="deaths_cohort_pod.csv", path = here::here("output"))

# Number of deaths by quarter and place of death

deaths_quarter_pod <- df_input %>%
    group_by(dod_quarter, pod_ons) %>%
    summarise(deaths = n())

write_csv(deaths_quarter_pod, filename ="deaths_quarter_pod.csv", path = here::here("output"))

################################################################################

########## Plot death counts by place ##########

# Plot of number of deaths by place and cohort

plot_pod_cohort <- ggplot(deaths_cohort_pod) + 
    geom_bar(aes(x = reorder(pod_ons, deaths), y = deaths, fill = factor(cohort, levels = c("0", "1")))
        , stat = "identity", position = "dodge", width = 0.6) +
    coord_flip() +
    labs(x = "Place", y = "Number of deaths") +
    scale_y_continuous(limits = c(0, 40), expand = c(0, 0)) +
    #scale_x_discrete(labels = c("home" = "Home")) +
    scale_fill_manual(values = c("#00C27A", "#9F67FF"), labels = c("Pre-pandemic", "Pandemic")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "#9AA0AA", size = 0.3),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),

        plot.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
        plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit = "cm"), colour = "#271544", size = 10, face = "bold", hjust = 0),
        plot.title.position = "plot",

        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "#9AA0AA", size = 8, family = "sans", angle = 45, hjust=1),
        axis.text.y = element_text(colour = "#9AA0AA", size = 8, family = "sans", hjust = 1),
        axis.title.x = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold", angle = 90),

        legend.key.size = unit(0.4, "cm")
)

ggsave(plot = plot_pod_cohort, filename ="pod_cohort.png", path = here::here("output"))
    
# Plot of proportion of deaths by place and cohort

plot_pod_cohort_prop <- ggplot(deaths_cohort_pod) + 
    geom_bar(aes(x = reorder(pod_ons, proportion), y = proportion, fill = factor(cohort, levels = c("0", "1")))
        , stat = "identity", position = "dodge", width = 0.6) +
    coord_flip() +
    labs(x = "Place", y = "Number of deaths") +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    #scale_x_discrete(labels = c("home" = "Home")) +
    scale_fill_manual(values = c("#00C27A", "#9F67FF"), labels = c("Pre-pandemic", "Pandemic")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
        panel.border = element_blank(),
        panel.grid.major.x = element_line(colour = "#9AA0AA", size = 0.3),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),

        plot.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
        plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),
        plot.title = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit = "cm"), colour = "#271544", size = 10, face = "bold", hjust = 0),
        plot.title.position = "plot",

        axis.ticks = element_blank(),
        axis.text.x = element_text(colour = "#9AA0AA", size = 8, family = "sans", angle = 45, hjust=1),
        axis.text.y = element_text(colour = "#9AA0AA", size = 8, family = "sans", hjust = 1),
        axis.title.x = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold", angle = 90),

        legend.key.size = unit(0.4, "cm")
)

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
    pivot_wider(names_from = cohort, names_prefix = "cohort", values_from = deaths) %>%
    mutate(ratio = cohort_1 / cohort_0)
    
write_csv(deaths_ratio_pod, filename ="deaths_ratio_pod.csv", path = here::here("output"))

#  Ratio - pod * sex

deaths_ratio_pod_sex <- df_input %>%
    group_by(cohort, pod_ons, sex) %>%
    summarise(deaths = n()) %>%
    pivot_wider(names_from = cohort, names_prefix = "cohort", values_from = deaths) %>%
    mutate(ratio = cohort_1 / cohort_0)

write_csv(deaths_ratio_pod_sex, filename ="deaths_ratio_pod_sex.csv", path = here::here("output"))

################################################################################
