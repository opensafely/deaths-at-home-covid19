################################################################################

########## FORMAT LOOKUPS ##########

################################################################################

# Save prepared lookups to repository
# - MSOA > LAD > RGN
# - LAD > IMD decile

################################################################################

########## Libraries ##########

library("tidyverse")
library("janitor")
library("geojsonio")
library("tidylog")

################################################################################

########## Save locations ##########

fs::dir_create(here::here("docs", "data_downloads_temp"))
fs::dir_create(here::here("docs", "lookups"))

################################################################################

########## MSOA > LAD > RGN ##########

filename1 <- "docs/data_downloads_temp/oa_lsoa_msoa_la_2020.geojson"
url1 <- "https://opendata.arcgis.com/datasets/65664b00231444edb3f6f83c9d40591f_0.geojson"

download.file(url1, destfile = filename1, mode = "wb")

msoa_lad_rgn <- as_tibble(geojson_read(filename1, parse = TRUE, what = "list")$features$properties) %>%
  clean_names() %>%
  distinct(msoa11cd, lad20cd, rgn20cd)

write_csv(msoa_lad_rgn, here::here("docs", "lookups", "msoa_lad_rgn_2020.csv"))

################################################################################

########## LAD > IMD quintile ##########

# Re-average IMD average rank to align to 2020 local authorities

filename2 <- "docs/data_downloads_temp/lad_imd_2019.xlsx"
url2 <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/833995/File_10_-_IoD2019_Local_Authority_District_Summaries__lower-tier__.xlsx"

download.file(url2, destfile = filename2, mode = "wb")

lad_imd <- readxl::read_xlsx(filename2, sheet = "IMD") %>%
  clean_names() %>%
  mutate(lad20cd = case_when(local_authority_district_code_2019 %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060"
                             , TRUE ~ local_authority_district_code_2019)) %>%
  group_by(lad20cd) %>%
  summarise(imd_average_rank = mean(imd_average_rank, na.rm = TRUE)) %>%
  mutate(imd19_quintile = case_when(imd_average_rank < quantile(imd_average_rank, 0.2) ~ 5
                                  , imd_average_rank >= quantile(imd_average_rank, 0.2) & imd_average_rank < quantile(imd_average_rank, 0.4) ~ 4
                                  , imd_average_rank >= quantile(imd_average_rank, 0.4) & imd_average_rank < quantile(imd_average_rank, 0.6) ~ 3
                                  , imd_average_rank >= quantile(imd_average_rank, 0.6) & imd_average_rank < quantile(imd_average_rank, 0.8) ~ 2
                                  , TRUE ~ 1)) %>%
  select(lad20cd, imd19_quintile)

write_csv(lad_imd, here::here("docs", "lookups", "lad_imd_2019.csv"))

################################################################################

########## Erase temporary data folder ##########

fs::dir_delete(here::here("docs", "data_downloads_temp"))

################################################################################
