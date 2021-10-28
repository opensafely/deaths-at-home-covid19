library("tidyverse")

df_input <- read_csv(
  here::here("output", "input.csv"),
  col_types = cols(patient_id = col_integer(),age = col_double())
)

plot_age <- ggplot(data=df_input, aes(df_input$age)) + 
geom_histogram() +
theme_bw()

ggsave(
  plot = plot_age,
  filename ="histogram.png", path = here::here("output"),
)