#-------------------------------------------------------------------------------
# Convert eol_report to html
# Date: 31.07.2023
# Author: Eilís
# Aim: Create an html file of Rmd doc
#-------------------------------------------------------------------------------


# Render Rmd file ---------------------------------------------------------

rmarkdown::render(here::here("analysis", "eol_service_report.Rmd"), 
                  output_file = "eol_report.html", output_dir = "output/os_reports/eol_service")

