
service_use_cohort_home <- tidyr::expand_grid(measure = unique(df_input %>%
                                                                 select(ends_with("_1m"), ends_with("_3m"), ends_with("_1y")) %>%
                                                                 select(-contains("gp_hist")) %>% 
                                                                 pivot_longer(cols = everything(), names_to = "measure", values_to = "value") %>%
                                                                 select(measure))$measure
                                              , char_category = unique(df_input %>%
                                                                         select(all_of(characteristics)) %>%
                                                                         mutate(across(everything(), as_factor)) %>%
                                                                         pivot_longer(cols = everything(), names_to = "characteristic", values_to = "category") %>%
                                                                         mutate(char_category = paste0(characteristic, "_", category)) %>%
                                                                         select(char_category))$char_category) %>%
  mutate(characteristic = str_extract(char_category, "[^_]+")
         , category = str_extract(char_category, "[^_]+$")) 
