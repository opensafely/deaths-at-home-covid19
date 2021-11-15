library("tidyverse")

df_input <- read_csv(
  here::here("output", "input.csv"),
  col_types = cols(patient_id = col_integer(), age_death = col_double())
)

plot_age <- ggplot(data=df_input, aes(df_input$age_death)) + 
  geom_histogram(fill = "#9F67FF") +
  labs(x = "Age at death", y = "Number of people") +
  scale_y_continuous(limits = c(0, 80)) +
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
    axis.text.x = element_text(colour = "#9AA0AA", size = 8, family = "sans"),
    axis.text.y = element_text(colour = "#9AA0AA", size = 8, family = "sans"),
    axis.title.x = element_text(margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold"),
    axis.title.y = element_text(margin = margin(t = 0, r = 0.3, b = 0, l = 0, unit = "cm"), colour = "#271544", size = 8, face = "bold", angle = 90),
    plot.title = element_text(margin = margin(t = 0, r = 0, b = 0.3, l = 0, unit = "cm"), colour = "#271544", size = 10, face = "bold", hjust = 0),
    plot.title.position = "plot"
  )

ggsave(
  plot = plot_age,
  filename ="histogram.png", path = here::here("output"),
)