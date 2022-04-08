################################################################################

########## PRACTICE MEASURES CHARTS ##########

################################################################################

# Create decile charts to show distribution of practice measures by month
# Funnel plots?

################################################################################

########## Libraries ##########

library("tidyverse")
library("lubridate")

################################################################################

########## Save location ##########

fs::dir_create(here::here("output", "practice_measures", "charts"))

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

########## Decile charts ########## 

plots <- tibble(file_list = list.files(here::here("output", "practice_measures"))) %>%
  filter(str_detect(file_list, "^measure") & str_detect(file_list, "by_practice.csv$")) %>%
  mutate(input_table = map(file_list, function(file_list) read_csv(here::here("output", "practice_measures", file_list)) %>%
                         group_by(date) %>%
                         summarise(value = quantile(value, seq(0.1, 0.9, 0.1), na.rm = TRUE)
                                   , decile = seq(0.1, 0.9, 0.1)))
         , input_label = str_sub(file_list, 9, -5)
         , plot = map2(input_table, input_label, function(input_table, input_label) ggsave(ggplot() +
                         geom_line(input_table
                                   , mapping = aes(x = date, y = value, group = factor(decile)), colour = "#556370", linetype = "dashed") +
                         geom_line(data = input_table %>% filter(decile == 0.5)
                                   , mapping = aes(x = date, y = value, group = factor(decile)), colour = "#9F67FF", lwd = 1) +
                         guides() +
                         labs(title = input_label, x = "Study month", y = "Decile value") +
                         scale_x_date(expand = c(0, 0), date_breaks = "month", date_labels = "%b %Y") +
                         scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
                         NT_style() +
                         theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
                         , filename = paste0("deciles_", input_label, ".png"), path = here::here("output", "practice_measures", "charts"))))

################################################################################

##########  ########## 



################################################################################
