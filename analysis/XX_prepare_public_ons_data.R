################################################################################

########## PREPARE PUBLIC ONS DATA ##########

################################################################################

# Format tables from published ONS data to compare with our cohort

################################################################################

########## Libraries ##########

library("tidyverse")
library("lubridate")
library("janitor")

################################################################################

########## Key functions ##########

`%notin%` <- Negate(`%in%`)

################################################################################

########## Save locations ##########

fs::dir_create(here::here("docs", "data_downloads_temp"))
fs::dir_create(here::here("docs", "ons_comparison_data"))

################################################################################

########## Download and save ##########

# ONS mortality analysis file

filename <- "docs/data_downloads_temp/monthlymortalityanalysisdec.xlsx"
url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlymortalityanalysisenglandandwales%2fdecember2021/monthlymortalityanalysisdec.xlsx"

download.file(url, destfile = filename, mode = "wb")

################################################################################

########## Import tables to R and format ##########

## Table 1 - sex

table1_data <- readxl::read_xlsx(filename, sheet = "Table 1", col_names = FALSE, skip = 5, n_max = 252)

table1_header1 <- readxl::read_xlsx(filename, sheet = "Table 1", col_names = FALSE, skip = 3, n_max = 2)

table1_header1 <- table1_header1 %>%
  t() %>%
  as.data.frame() %>%
  fill(., "V1")
table1_header1 <- as.character(table1_header1$V1)

table1_header2 <- readxl::read_xlsx(filename, sheet = "Table 1", col_names = FALSE, skip = 4, n_max = 1)
table1_header2 <- unname(unlist(table1_header2[1, ]))

table1_column_names <- str_c(str_replace_na(table1_header1, "placeholder"), str_replace_na(table1_header2, "placeholder"), sep = "_")

names(table1_data) <- tolower(table1_column_names)
names(table1_data) <- gsub("placeholder", "", names(table1_data))
table1_data <- table1_data %>%
  clean_names() 

table1 <- table1_data %>%
  mutate(period = as_date(paste0("1 ", period), format = "%d %B %Y")) %>%
  filter(period >= as_date("2019-03-01") & period <= as_date("2021-02-01")) %>%
  select(period, males_number_of_deaths, females_number_of_deaths) %>%
  pivot_longer(cols = -c(period), names_to = "sex", values_to = "ons_deaths") %>%
  mutate(sex = toupper(str_sub(sex, 1, 1)))

write_csv(table1, here::here("docs", "ons_comparison_data", "table1_sex_onsmortality.csv"))

#######################################

## Table 4 and 8c - age group

table4_data <- readxl::read_xlsx(filename, sheet = "Table 4", col_names = FALSE, skip = 5, n_max = 252)

table4_header1 <- readxl::read_xlsx(filename, sheet = "Table 4", col_names = FALSE, skip = 3, n_max = 2)

table4_header1 <- table4_header1 %>%
  t() %>%
  as.data.frame() %>%
  fill(., "V1")
table4_header1 <- as.character(table4_header1$V1)

table4_header2 <- readxl::read_xlsx(filename, sheet = "Table 4", col_names = FALSE, skip = 4, n_max = 1)
table4_header2 <- unname(unlist(table4_header2[1, ]))

table4_column_names <- str_c(str_replace_na(table4_header1, "placeholder"), str_replace_na(table4_header2, "placeholder"), sep = "_")

names(table4_data) <- tolower(table4_column_names)
names(table4_data) <- gsub("placeholder", "", names(table4_data))
table4_data <- table4_data %>%
  clean_names() 

table4 <- table4_data %>%
  mutate(period = as_date(paste0("1 ", period), format = "%d %B %Y")) %>%
  filter(period >= as_date("2019-03-01") & period <= as_date("2021-02-01")) %>%
  select(period, people_0_to_74_years_number_of_deaths) %>%
  rename(ons_deaths = people_0_to_74_years_number_of_deaths) %>%
  mutate(agegrp = "<75")

table8c_data <- readxl::read_xlsx(filename, sheet = "Table 8c", col_names = FALSE, skip = 5, n_max = 252)

table8c_header1 <- readxl::read_xlsx(filename, sheet = "Table 8c", col_names = FALSE, skip = 3, n_max = 2)

table8c_header1 <- table8c_header1 %>%
  t() %>%
  as.data.frame() %>%
  fill(., "V1")
table8c_header1 <- as.character(table8c_header1$V1)

table8c_header2 <- readxl::read_xlsx(filename, sheet = "Table 8c", col_names = FALSE, skip = 4, n_max = 1)
table8c_header2 <- unname(unlist(table8c_header2[1, ]))

table8c_column_names <- str_c(str_replace_na(table8c_header1, "placeholder"), str_replace_na(table8c_header2, "placeholder"), sep = "_")

names(table8c_data) <- tolower(table8c_column_names)
names(table8c_data) <- gsub("placeholder", "", names(table8c_data))
table8c_data <- table8c_data %>%
  clean_names() 

table8c <- table8c_data %>%
  mutate(period = as_date(paste0("1 ", period), format = "%d %B %Y")) %>%
  filter(period >= as_date("2019-03-01") & period <= as_date("2021-02-01")) %>%
  select(period, ends_with("_deaths")) %>%
  pivot_longer(cols = -c(period), names_to = "agegrp", values_to = "ons_deaths") %>%
  mutate(agegrp = case_when(str_sub(agegrp, 8, -18) != "90_and_over" ~ str_replace_all(str_sub(agegrp, 8, -18), "_", "-")
                            , str_sub(agegrp, 8, -18) == "90_and_over" ~ "90+"))

table4_8c <- table4 %>%
  bind_rows(table8c)

write_csv(table4_8c, here::here("docs", "ons_comparison_data", "table4_8c_agegrp_onsmortality.csv"))

#######################################

## Table 14a - place of death

table14a_data <- readxl::read_xlsx(filename, sheet = "Table 14a", col_names = FALSE, skip = 5, n_max = 24)

table14a_header <- readxl::read_xlsx(filename, sheet = "Table 14a", col_names = FALSE, skip = 3, n_max = 2)

table14a_header <- table14a_header %>%
  t() %>%
  as.data.frame() %>%
  fill(., "V1")
table14a_header1 <- as.character(table14a_header$V1)
table14a_header2 <- as.character(table14a_header$V2)

table14a_column_names <- str_c(str_replace_na(table14a_header1, "placeholder"), str_replace_na(table14a_header2, "placeholder"), sep = "_")

names(table14a_data) <- tolower(table14a_column_names)
names(table14a_data) <- gsub("placeholder", "", names(table14a_data))
table14a_data <- table14a_data %>%
  clean_names() 

table14a <- table14a_data %>%
  mutate(period = as_date(paste0("1 ", month_of_occurrence), format = "%d %B %Y")) %>%
  filter(period >= as_date("2019-03-01") & period <= as_date("2021-02-01")) %>%
  select(period, starts_with("x2020_2021_")) %>%
  pivot_longer(cols = -c(period), names_to = "place_of_death", values_to = "ons_deaths") %>%
  mutate(place_of_death = paste0(toupper(str_sub(place_of_death, 12, 12)), 
                                 str_replace_all(str_replace_all(str_sub(place_of_death, 13), "[:digit:]", ""), "_", " ")))

write_csv(table14a, here::here("docs", "ons_comparison_data", "table14a_pod_onsmortality.csv"))

#######################################

## Table 11a - cause of death

# Monthly deaths (Jan 20 - Feb 21) by leading (top 10) cause of death - Table 11a
# Comes from several iterations of the monthly mortality analysis files

# January to June 2020 file

jan_jun20 <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12656provisionalleadingcausesofdeathbymonthofdeathregistrationandcountryenglandandwalesdeathsregisteredjanuarytojune2020/leadingcausesbymonth.xls"

download.file(jan_jun20, "docs/data_downloads_temp/monthlymortalityanalysisjanuary_june_2020.xls", mode="wb")

table11a_data <- readxl::read_xls("docs/data_downloads_temp/monthlymortalityanalysisjanuary_june_2020.xls", sheet = "England", col_names = TRUE, skip = 13, n_max = 70) %>%
  clean_names() %>%
  rename(cause = top_ten_leading_causes_of_death_in_january_2020) %>%
  mutate(month = case_when(cause %in% c("February", "March", "April", "May", "June") ~ cause
                           , TRUE ~ NA_character_)) %>%
  fill(month, .direction = "down") %>%
  mutate(month = case_when(is.na(month) ~ "January"
                           , TRUE ~ month)) %>%
  select(cause, rank, number_of_deaths, month) %>%
  group_split(month)

for(i in seq_along(table11a_data)){ 
  
  month_label <- paste0(tolower(table11a_data[[i]]$month), "_2020")
  
  file <- table11a_data[[i]] %>%
    filter(cause %notin% c("January", "February", "March", "April", "May", "June") & !str_detect(cause, "Top ten"))  %>%
    mutate(period = as_date(paste0("1-", month, "-2020"), format = "%d-%B-%Y")
           , number_of_deaths = as.integer(number_of_deaths)) %>%
    arrange(desc(number_of_deaths)) %>%
    mutate(rank_in = row_number()) %>%
    select(period, cause, rank_in, number_of_deaths)
  
  assign(paste0("table11a_", month_label), file)
  
}

# July to September 2020 files

path <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlymortalityanalysisenglandandwales%2f"

jul_20 <- paste0(path, "july2020/monthlymortalityanalysisjuly.xlsx")
aug_20 <- paste0(path, "august2020/monthlymortalityanalysisaugust.xlsx")
sep_20 <- paste0(path, "september2020/monthlymortalityanalysisseptember3.xlsx")
oct_20 <- paste0(path, "october2020/monthlymortalityanalysisoctober.xlsx")
nov_20 <- paste0(path, "november2020/monthlymortalityanalysisnovember.xlsx")
dec_20 <- paste0(path, "december2020/monthlymortalityanalysisdecember.xlsx")
jan_21 <- paste0(path, "january2021/monthlymortalityanalysisjanuary20214.xlsx")
feb_21 <- paste0(path, "february2021/monthlymortalityanalysisfebruary2021.xlsx")

urls1 <- c(jul_20, aug_20, sep_20)
months1 <- c("july_2020" ,"august_2020", "september_2020")
sheets1 <- c("Table 12", "Table 11", "Table 11")

#Map(function(u, d) download.file(u, d, mode="wb"), urls, paste0("docs/data_downloads_temp/monthlymortalityanalysis", destinations, ".xlsx"))

for(i in seq_along(urls1)){
  
  download.file(urls1[i], paste0("docs/data_downloads_temp/monthlymortalityanalysis", months1[i], ".xlsx"), mode="wb")
  
  table11a_data <- readxl::read_xlsx(paste0("docs/data_downloads_temp/monthlymortalityanalysis", months1[i], ".xlsx"), sheet = sheets1[i], col_names = FALSE, skip = 6, n_max = 10)

  table11a_header <- readxl::read_xlsx(paste0("docs/data_downloads_temp/monthlymortalityanalysis", months1[i], ".xlsx"), sheet = sheets1[i], col_names = FALSE, skip = 3, n_max = 2)
  
  table11a_header <- table11a_header %>%
    t() %>%
    as.data.frame() %>%
    fill(., "V1")
  
  table11a_header1 <- as.character(table11a_header$V1)
  table11a_header2 <- as.character(table11a_header$V2)
  
  table11a_column_names <- str_c(str_replace_na(table11a_header1, "placeholder"), str_replace_na(table11a_header2, "placeholder"), sep = "_")
  
  names(table11a_data) <- tolower(table11a_column_names)
  names(table11a_data) <- gsub("placeholder", "", names(table11a_data))
  
  table11a_data <- table11a_data %>%
    clean_names() %>%
    select(leading_cause, matches(months1[i])) %>%
    rename(cause = leading_cause)
  
  names(table11a_data) <- gsub(months1[i], "", names(table11a_data))
  
  table11a <- table11a_data %>% 
    clean_names() %>%
    mutate(period = as_date(paste0("1_", months1[i]), format = "%d_%B_%Y")) %>%
    arrange(desc(number_of_deaths)) %>%
    mutate(rank_in = row_number()) %>%
    select(period, cause, rank_in, number_of_deaths)
  
  assign(paste0("table11a_", months1[i]), table11a)
    
}

# October 2020 to Febraury 2021 files

urls2 <- c(oct_20, nov_20, dec_20, jan_21, feb_21)
months2 <- c("october_2020", "november_2020", "december_2020", "january_2021", "february_2021")

for(i in seq_along(urls2)){
  
  download.file(urls2[i], paste0("docs/data_downloads_temp/monthlymortalityanalysis", months2[i], ".xlsx"), mode="wb")
  
  table11a_data <- readxl::read_xlsx(paste0("docs/data_downloads_temp/monthlymortalityanalysis", months2[i], ".xlsx"), sheet = "Table 11a", col_names = FALSE, skip = 6, n_max = 10)
  
  table11a_header <- readxl::read_xlsx(paste0("docs/data_downloads_temp/monthlymortalityanalysis", months2[i], ".xlsx"), sheet = "Table 11a", col_names = FALSE, skip = 3, n_max = 2)
  
  table11a_header <- table11a_header %>%
    t() %>%
    as.data.frame() %>%
    fill(., "V1")
  
  table11a_header1 <- as.character(table11a_header$V1)
  table11a_header2 <- as.character(table11a_header$V2)
  
  table11a_column_names <- str_c(str_replace_na(table11a_header1, "placeholder"), str_replace_na(table11a_header2, "placeholder"), sep = "_")
  
  names(table11a_data) <- tolower(table11a_column_names)
  names(table11a_data) <- gsub("placeholder", "", names(table11a_data))
  
  table11a_data <- table11a_data %>%
    clean_names() %>%
    select(cause, matches(months2[i]))
  
  names(table11a_data) <- gsub(months2[i], "", names(table11a_data))
  
  table11a <- table11a_data %>% 
    clean_names() %>%
    mutate(period = as_date(paste0("1_", months2[i]), format = "%d_%B_%Y")) %>%
    arrange(desc(number_of_deaths)) %>%
    mutate(rank_in = row_number()) %>%
    select(period, cause, rank_in, number_of_deaths)
  
  assign(paste0("table11a_", months2[i]), table11a)
  
}

# March 2019 to December 2019 file
# Deaths by date of occurrence rather than registration

download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12060deathsbyleadingcausesgroupingsymonthofoccurrenceengland2015to2019/deathsbyleadingcauses20152019.xlsx"
              , destfile = "docs/data_downloads_temp/deathsbyleadingcauses20152019.xlsx", mode = "wb")

leadingcause19_data <- readxl::read_xlsx("docs/data_downloads_temp/deathsbyleadingcauses20152019.xlsx", sheet = "2019", col_names = TRUE, skip = 2, n_max = 68) %>%
  clean_names() %>% 
  pivot_longer(cols = -c(cause_of_death), names_to = "month", values_to = "number_of_deaths") %>% 
  mutate(period = as_date(paste0("01-", month, "-2019"), format = "%d-%B-%Y")) %>%
  rename(cause = cause_of_death) %>% 
  group_by(period) %>%
  arrange(desc(number_of_deaths)) %>%
  mutate(rank_in = row_number()) %>%
  select(period, cause, rank_in, number_of_deaths)

# Join all months together
# Remove trailing white space from cause
# Align cause names to ICD10 cause of death lookup - lowercase to avoid any issues
table11a <- bind_rows(leadingcause19_data, table11a_january_2020, table11a_february_2020, table11a_march_2020, table11a_april_2020, table11a_may_2020, table11a_june_2020 ,table11a_july_2020, table11a_august_2020, table11a_september_2020, table11a_october_2020, table11a_november_2020
                      , table11a_december_2020, table11a_january_2021, table11a_february_2021) %>%
  mutate(cause = trimws(cause, "right")
         , cause = case_when(cause == "Dementia and Alzheimer's disease" ~ "Dementia and Alzheimer disease"
                             , TRUE ~ cause)
         , cause = tolower(cause)) %>% 
  rename(ons_deaths = number_of_deaths)

write_csv(table11a, here::here("docs", "ons_comparison_data", "table11a_cod_onsmortality.csv"))

#######################################

## Deaths by region and month

#2019
download.file("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlyfiguresondeathsregisteredbyareaofusualresidence%2f2019/annual2019publishedoutputrefresh.xls"
              , destfile = "docs/data_downloads_temp/annual2019publishedoutputrefresh.xls", mode = "wb")

#2020
download.file("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlyfiguresondeathsregisteredbyareaofusualresidence%2f2020/annual2020publishedoutputrefresh.xls"
              , destfile = "docs/data_downloads_temp/annual2020publishedoutputrefresh.xls", mode = "wb")

#2021
download.file("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlyfiguresondeathsregisteredbyareaofusualresidence%2f2021/deathsregisteredmonthlyusualareaofresidenceenglandandwales.xlsx"
              , destfile = "docs/data_downloads_temp/deathsregisteredmonthlyusualareaofresidenceenglandandwales2021.xlsx", mode = "wb")

temp <- readxl::read_xls("docs/data_downloads_temp/annual2019publishedoutputrefresh.xls", sheet = "Figures for 2019")


region <- readxl::read_xls("docs/data_downloads_temp/annual2019publishedoutputrefresh.xls", sheet = "Figures for 2019", col_names = TRUE, skip = 3, n_max = 440) %>% 
  clean_names() %>% 
  filter(str_detect(area_of_usual_residence, "^E12")) %>%
  pivot_longer(cols = -c(area_of_usual_residence, x2), names_to = "month", values_to = "ons_deaths") %>%
  mutate(period = as_date(paste0("01_", month), format = "%d_%b_%y")) %>%
  rename(region = area_of_usual_residence) %>%
  select(period, region, ons_deaths) %>%
  bind_rows(readxl::read_xls("docs/data_downloads_temp/annual2020publishedoutputrefresh.xls", sheet = "Figures for 2020", col_names = TRUE, skip = 3, n_max = 433) %>% 
              clean_names() %>% 
              filter(str_detect(area_of_usual_residence, "^E12")) %>%
              pivot_longer(cols = -c(area_of_usual_residence, x2), names_to = "month", values_to = "ons_deaths") %>%
              mutate(period = as_date(paste0("01_", month), format = "%d_%b_%y")) %>%
              rename(region = area_of_usual_residence) %>%
              select(period, region, ons_deaths)) %>% 
  bind_rows(readxl::read_xlsx("docs/data_downloads_temp/deathsregisteredmonthlyusualareaofresidenceenglandandwales2021.xlsx", sheet = "Figures for 2021", col_names = TRUE, skip = 4, n_max = 378) %>% 
              clean_names() %>% 
              filter(str_detect(area_of_usual_residence, "^E12")) %>%
              pivot_longer(cols = -c(area_of_usual_residence, x2), names_to = "month", values_to = "ons_deaths") %>%
              mutate(period = as_date(paste0("01_", month), format = "%d_%b_%y")) %>%
              rename(region = area_of_usual_residence) %>%
              select(period, region, ons_deaths)) %>%
  filter(period >= as_date("2019-03-01") & period <= as_date("2021-02-01"))

write_csv(region, here::here("docs", "ons_comparison_data", "region_onsmortality.csv"))

################################################################################

########## Erase temporary data folder ##########

fs::dir_delete(here::here("docs", "data_downloads_temp"))

################################################################################
