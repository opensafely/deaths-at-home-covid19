########## TEST OPENSAFELY OUTPUT PROCESS ##########

# Make a date of death frequency plot

########## Libraries ##########

library("tidyverse")

########## Import data ##########

df_input <- arrow::read_feather(file = here::here("output", "input.feather")) %>%
mutate(dod_ons = as.Date(dod_ons, format = "%Y-%m-%d"))

########## Make plot ##########

plot_dod <- ggplot(df_input, aes(dod_ons)) + 
  geom_histogram(fill = "#9F67FF") +
  labs(x = "Date of death (ONS)", y = "Number of people") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y", limits = c(as.Date("2019-03-01"), as.Date("2021-02-28")), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#F4F4F4", colour = "#F4F4F4"),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.5, unit = "cm"),

    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "#9AA0AA"),
    panel.grid.minor = element_blank(),

    axis.ticks = element_blank(),
    axis.text.x = element_text(colour = "#9AA0AA", size = 8, family = "sans", angle = 45, hjust=1),
    axis.text.y = element_text(colour = "#9AA0AA", size = 8, family = "sans"),
    axis.title.x = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold"),
    axis.title.y = element_text(margin = margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold", angle = 90),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit = "cm"), colour = "#271544", size = 10, face = "bold", hjust = 0),
    plot.title.position = "plot"
  )

########## Save plot ##########

ggsave(plot = plot_dod, filename ="dod_histogram.png", path = here::here("output"))
