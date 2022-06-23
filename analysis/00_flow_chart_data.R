################################################################################

########## FLOW CHART DATA ##########

################################################################################

# Create numbers for flow chart of exclusion

################################################################################

########## Libraries ##########

library("tidyverse")
library("lubridate")

################################################################################

########## Save locations ##########

fs::dir_create(here::here("output", "flow_chart_data"))

################################################################################

########## Import data ##########

flow_input <- arrow::read_feather(file = here::here("output", "input_flow_chart.feather"))

################################################################################

########## Create flow chart numbers ##########

total <- nrow(flow_input)

died <- nrow(flow_input %>%
  filter(has_died == TRUE))

registered_dod <- nrow(flow_input %>%
                         filter(has_died == TRUE & registered == TRUE))

valid_sex <-  nrow(flow_input %>%
                     filter(has_died == TRUE & registered == TRUE & (sex == "F" | sex == "M")))

flow <- tibble(total, died, registered_dod, valid_sex) %>% 
  pivot_longer(cols = everything(), names_to = "criteria", values_to = "value")

write_csv(flow, here::here("output", "flow_chart_data", "flow_chart_data.csv"))

################################################################################
