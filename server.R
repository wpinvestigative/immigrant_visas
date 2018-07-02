packages <- c("shiny", "tidyverse", "stringr", "lubridate", "DT")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(shiny)
library(tidyverse)
library(stringr)
library(lubridate)
library(DT)

# Loading data that was compiled by the script 01_combining_data.R
# The output .rds files from the output_data folder were moved into this folder where the ui.R and server.R scripts reside

countries <- readRDS("output_data/consolidated_long.rds")
regions <- readRDS("output_data/regional_long.rds")
nm <- readRDS("output_data/nm.rds")
cate <- readRDS("output_data/categories.rds")

# Generating a list of regions to use in the pulldown menu in ui.R
countries_region <- unique(countries$area)

# Generating a list of visas to use in the pulldown menus
visa_type <- unique(countries$type)

# renaming a column in the monthly figures so it can be joined with another data frame later on
nm <- dplyr::rename(nm, Country=`Foreign State of Chargeability or Place of Birth`)

# Don't need the grand total columns
nm <- filter(nm, type!="Grand Total")

# Generating a list of visa types based on the monthly figures data frame
visa_type_nm <- unique(nm$type)

# A custom function that will deal with rounding numbers
round = function(x) ifelse(x>0, trunc(x+0.5), trunc(x-0.5));

# Generating a list of countries based on the monthly figures data frame
nm_countries <- unique(nm$Country)

# Generating a list of countries to use in the pulldown menu in ui.R
ye_countries <- unique(countries$`Foreign State`)

# Prepping the countries data frame so it can focus on regions
countries_regions <- select(countries, `Foreign State`, area) %>% ungroup() %>% unique
countries_regions$year <- NULL
countries_regions <- unique(countries_regions)

nmc <- data.frame(nm_countries)
colnames(nmc) <- "Country"
nmc$type <- "Monthly"

yec <- data.frame(ye_countries)
colnames(yec) <- "Country"

cnames <- full_join(nmc, yec)

# Creating 2018 data to join to the annual figures by creating a projected figure from the available monthly data
nm_projected <- nm %>% 
  mutate(type=gsub("Iraqis/Afghans who helped US abroad", "Special Immigrants", type)) %>% 
  group_by(Country, date, type) %>% 
  summarize(Issuances=sum(Issuances, na.rm=T)) %>% 
  group_by(Country, type) %>% 
  ## if basing on 8 months of available data
  summarize(total=round(sum(Issuances)/8*12)) %>%
  dplyr::rename(`Foreign State`=Country) %>% 
  left_join(countries_regions) %>% 
  mutate(year=2018) %>% 
  select(`Foreign State`, year, area, type, total)

# Adding projected 2018 to annual data
countries <- rbind(countries, nm_projected) %>% ungroup()

# more cleaning
nm_projected2 <- select(nm_projected, `Foreign State`, year, area, type, total) %>% 
  summarize(what=sum(total, na.rm=T)) %>% 
  mutate(type="Total", year=2018) %>%
  left_join(countries_regions) %>% 
  select(`Foreign State`, year, area, type, total=what)

countries <- rbind(countries, nm_projected2)

# spreading out the dataframe so it works in a wide table
# also renaming columns so they look nice and readable in the data table

countries_wide <- spread(countries, year, total) %>% 
  mutate(`Percent change '09 - '16'` =round((`2016`-`2009`)/`2009`*100),
         `Percent change '16 - '18'` =round((`2018`-`2016`)/`2016`*100))


# spreading out the dataframe so it works in a wide table
# also renaming columns so they look nice and readable in the data table

visa_types_summary <- countries %>% 
  group_by(year, type) %>% 
  summarize(total=sum(total)) %>% 
  spread(year, total) %>% 
  mutate(`Percent change '09 - '16'` =round((`2016`-`2009`)/`2009`*100),
         `Percent change '16 - '18'` =round((`2018`-`2016`)/`2016`*100))


shinyServer(function(input, output) {
  
  
## Plot under Countries - Chart
  output$top_chart <- renderPlot({
    
    countries %>% 
      filter(`Foreign State` %in% input$CountryPicker) %>% 
      filter(type %in% input$VisaPicker) %>% 
      ggplot(aes(x=as.factor(year), y=total, group=`Foreign State`, color=`Foreign State`)) +
      geom_line() +
      facet_wrap(~type)
  })
  
  
## Table under Countries - Table
  output$top_table <- renderDataTable(
    
    countries %>% 
      filter(`Foreign State` %in% input$CountryPicker) %>% 
      filter(type %in% input$VisaPicker) %>% 
      spread(year, total) %>% 
      ungroup() %>% 
      select(-area)
  )
  
## Plot under Region - Chart
  output$region_chart <- renderPlot({
    
    countries %>% 
      filter(area %in% input$RegionPicker) %>% 
      filter(type %in% input$VisaPicker2) %>% 
      group_by(year, area, type) %>% 
      summarize(total=sum(total, na.rm=T)) %>% 
      ggplot(aes(x=as.factor(year), y=total, group=area, color=area)) +
      geom_line() +
      facet_wrap(~type)
  })

## Plot under Region - Table
  output$region_table <- renderDataTable(
    
    countries %>% 
      filter(area %in% input$RegionPicker) %>% 
      filter(type %in% input$VisaPicker2) %>% 
      group_by(year, area, type) %>% 
      summarize(total=sum(total, na.rm=T))%>% 
      spread(year, total)
  )
  
## Plot under October - May - Chart
  output$nm_chart <- renderPlot({
    
    nm %>% 
      filter(Country %in% input$CountryPickerNM) %>% 
      filter(type %in% input$VisaPicker3) %>% 
      ggplot(aes(x=date, y=Issuances, group=Country, fill=Country)) +
      geom_col(position="dodge", stat="identity") +
      facet_wrap(~type)
  })
  
## Table under October - May - Table
  output$nm_table <- renderDataTable(
    
    nm %>% 
      filter(Country %in% input$CountryPickerNM) %>% 
      filter(type %in% input$VisaPicker3) %>% 
      spread(date, Issuances))
  
## Table under Calculations | Countries - Percent change 
  output$sum_table1 <- renderDataTable(
    select(countries_wide, `Foreign State`, area, type,
           `Percent change '09 - '16'`,
           `Percent change '16 - '18'`
           )
    ,
    filter="top")

## Table under Calculations | Visa types - Percent change
  
  output$visa_types1 <- renderDataTable(
    select(visa_types_summary,type,
           `Percent change '09 - '16'`,
           
           `Percent change '16 - '18'`)
    ,
    filter="top")
  
## Table under Calculations | Visa types - Percent change
  
  output$visa_types3 <- renderDataTable(
    
    select(visa_types_summary,type, `2009`, `2010`, `2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018 (projected)`=`2018`)
    
    ,filter="top")
  
  
  
})
