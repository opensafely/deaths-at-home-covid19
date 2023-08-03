#-------------------------------------------------------------------------------
# Charts for eol_service_report
# Date: 26.07.2023
# Author: Eilís
# Aim: Create png image files of charts for OpenSAFELY reports
#-------------------------------------------------------------------------------

# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)


# Create folder structure -------------------------------------------------

fs::dir_create("output", "os_reports", "eol_service")


# NT chart functions ------------------------------------------------------

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


# Code settings -----------------------------------------------------------

startdate <- dmy("01-06-2019")
enddate <- dmy("30-06-2023")


# Prepare data-------------------------------------------------------------

df <- arrow::read_feather(file = here::here("output", "os_reports", "input_os_reports.feather")) %>%
  mutate(dod_ons = as_date(dod_ons)
         , study_month = floor_date(dod_ons, unit = "month")
         , pod_ons_new = case_when(pod_ons == "Elsewhere" 
                                   | pod_ons == "Other communal establishment" ~ "Elsewhere/other"
                                   , TRUE ~ as_factor(pod_ons))) %>%
  filter(study_month >= startdate & study_month <= enddate)


# Deaths in period --------------------------------------------------------

# Number of deaths by month and place of death - including all deaths
deaths_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(count = n()) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(count = n()) %>%
              mutate(pod_ons_new = "All"))

write_csv(deaths_month, here::here("output", "os_reports", "eol_service", "deaths_month.csv"))

deaths_month_plot <- ggplot(deaths_month, aes(x = study_month, y = count
                                              , group = pod_ons_new
                                              , colour = pod_ons_new
                                              , fill = pod_ons_new)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "Month", y = "Number of deaths") +
  scale_colour_NT() +
  scale_fill_NT() +
  scale_x_date(expan = c(0,0), date_breaks = "3 months", date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0,0)
                     , limits = c(0, plyr::round_any(max(deaths_month$count)
                                                     , 20000, f = ceiling))
                     , breaks = seq(0
                                    , plyr::round_any(max(deaths_month$count)
                                                       , 20000, f = ceiling)
                                    , 20000)
                     , labels = scales::comma) +
  NT_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(deaths_month_plot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "deaths_month_plot.png"
       , path = here::here("output", "os_reports", "eol_service"))

# General practice interactions -------------------------------------------

# Mean GP interactions by month and place of death - including all deaths
gp_month <- df %>%
  group_by(study_month, pod_ons_new) %>%
  summarise(mean = mean(gp_1m, na.rm = TRUE)) %>%
  bind_rows(df %>%
              group_by(study_month) %>%
              summarise(mean = mean(gp_1m, na.rm = TRUE)) %>%
              mutate(pod_ons_new = "All"))

write_csv(gp_month, here::here("output", "os_reports", "eol_service", "gp_month.csv"))

gp_month_plot <- ggplot(gp_month, aes(x = study_month, y = mean
                                              , group = pod_ons_new
                                              , colour = pod_ons_new
                                              , fill = pod_ons_new)) +
  geom_line(size = 1) +
  geom_point(fill = "#F4F4F4", shape = 21, size = 1.5, stroke = 1.3) +
  guides(colour = guide_legend(nrow = 1)) +
  labs(x = "Month", y = "Average events per person") +
  scale_colour_NT() +
  scale_fill_NT() +
  scale_x_date(expan = c(0,0), date_breaks = "3 months", date_labels = "%b-%y") +
  scale_y_continuous(expand = c(0,0)
                     , limits = c(0, plyr::round_any(max(gp_month$mean)
                                                     , 1, f = ceiling))
                     , breaks = seq(0
                                    , plyr::round_any(max(gp_month$mean)
                                                      , 1, f = ceiling)
                                    , 1)
                     , labels = scales::comma) +
  NT_style() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

ggsave(gp_month_plot, dpi = 600, width = 20, height = 10, unit = "cm"
       , filename = "gp_month_plot.png"
       , path = here::here("output", "os_reports", "eol_service"))

