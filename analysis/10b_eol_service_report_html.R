#-------------------------------------------------------------------------------
# Convert eol_service_report to html
# Date: 31.07.2023
# Author: Eilís
# Aim: Create an html file from the Rmd doc
#-------------------------------------------------------------------------------


# Render Rmd file ---------------------------------------------------------

rmarkdown::render(here::here("analysis", "10a_eol_service_report.Rmd")
                  , output_file = "eol_service_report.html"
                  , output_dir = "output/os_reports/eol_service")
