## Data in this folder

* 2009_2017_visa_categories.xlsx
    * Parsed and stitched together from the raw PDF files as an Excel file, each category is a sheet
* 2009_2017_visa_categories_region.xlsx
    * Parsed and stitched together from the raw PDF files as an Excel file, each category is a sheet
* annual_visas_by_country0817.csv
    * Parsed and stitched together from the raw PDF files as a long CSV file
* annual_visas_by_region0817.csv
    * Parsed and stitched together from the raw PDF files as a long CSV file
* countries_long.csv
    * Long data frame of country, year, region, visa type, and total
* countries_regions.csv
    * Relationship file for official designated country-to-region designations
* categories_summary.csv
    * Summary of custom categories, such as "Travel Banned" and "Muslim-Majority Countries"
* countries_wide.csv
    * Wide data frame of country, year, region, visa type, and total
* specific_categories.csv
    * Relationship file for our self-designated country-to-type designations (which lead to categories.summary.csv)

----

## R specific files

Essentially the files above but in .rds file formats to be used with the Shiny app

* categories.rds
* consolidated_long.rds
* nm.rds
* regional_long.rds
    