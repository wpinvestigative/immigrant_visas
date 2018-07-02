[This data is published under an [Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) license](https://creativecommons.org/licenses/by-nc-sa/4.0/)]

## Immigrant Visas Explorer

----

R Shiny application to explore immigrant visa data from the U.S. State Department.

![](shiny_app.png)

The department [releases data](https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics.html) annually and monthly in PDFs.

The data has been scraped, cleaned and organized in a downloadable format along with the app.

The repo can be downloaded and explored manually. 

To run the app, you must have [R installed](https://cloud.r-project.org/) first.

**Via R terminal**

In the terminal, run

```
R "install.packages('shiny')"
R -e "shiny::runGitHub('immigrant_visas', 'wpinvestigative', launch.browser=TRUE)"
```

**Via RStudio**

The application can be run locally with the following command in [R Studio](https://www.rstudio.com/products/rstudio/download/#download): 

```
install.packages("shiny")
shiny::runGitHub("immigrant_visas", "wpinvestigative")
```

----

# About the folders in this repo

* **[scripts](scripts)** - Exploratory data analysis
* **[raw_data](raw_data)** - Raw data or consolidated spreadsheets from scraped PDFs
* **[output_data](output_data)** - Cleaned data transformed from raw data used for analysis and the Shiny App


