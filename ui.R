packages <- c("shiny", "tidyverse", "stringr", "shinythemes", "shinyWidgets", "DT")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

library(tidyverse)
library(shiny)
library(stringr)
library(shinythemes)
library(shinyWidgets)
library(DT)


# Loading data that was compiled by the script 01_combining_data.R
# The output .rds files from the output_data folder were moved into this folder where the ui.R and server.R scripts reside

countries <- readRDS("output_data/consolidated_long.rds")
regions <- readRDS("output_data/regional_long.rds")
nm <- readRDS("output_data/nm.rds")
cate <- readRDS("output_data/categories.rds")

## Prepping lists for pull down menu options
countries_list <- unique(countries$`Foreign State`)
countries_region <- unique(countries$area)
visa_type <- unique(countries$type)
nm <- dplyr::rename(nm, Country=`Foreign State of Chargeability or Place of Birth`)
countries_list_nm <- unique(nm$Country)
visa_type_nm <- unique(nm$type)
specific_categories <- unique(cate$TypeOf)


shinyUI(fluidPage(theme=shinytheme("flatly"),
                  
                  # Inserting the Washington Post Investigative logo
                  list(tags$head(HTML('<link rel="icon", href="https://avatars3.githubusercontent.com/u/29076131?s=30&v=4", 
                                      type="image/png" />'))),
                  div(style="padding: 1px 0px; width: '100%'",
                      titlePanel(
                        title="", windowTitle="Visas explorer"
                      )
                  ),
                  navbarPage(
                    title=div(HTML("<img src='https://avatars3.githubusercontent.com/u/29076131?s=30&v=4' hspace='5'>"), "Visas Explorer"),
                    tabPanel("Countries",
                             sidebarLayout(
                               sidebarPanel(
                                 h4("Select Country"),
                                 pickerInput(
                                   inputId = "CountryPicker", 
                                   label = NULL, 
                                   choices = countries_list, 
                                   selected = "Afghanistan",
                                   options = list(
                                     `actions-box` = TRUE, 
                                     size = 9,
                                     `selected-text-format` = "count > 3"
                                   ), 
                                   multiple = TRUE
                                 ),
                                 h4("Select Visa Type"),
                                 pickerInput(
                                   inputId = "VisaPicker", 
                                   label = NULL, 
                                   choices = visa_type, 
                                   selected = visa_type,
                                   options = list(
                                     `actions-box` = TRUE, 
                                     size = 9,
                                     `selected-text-format` = "count > 3"
                                   ), 
                                   multiple = TRUE
                                 ),
                                 p("Note: 2018 figure is projected number based on 8 months of data")
                                 
                               ),
                               
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel("Chart", 
                                            plotOutput("top_chart", height="600px")
                                            
                   
                                            
                                   ),
                                   tabPanel("Table",
                                            
                                            dataTableOutput("top_table")
                                            
                                   ))))
                    ),
                    tabPanel("Region",
                             sidebarLayout(
                               sidebarPanel(
                                 h4("Select Region"),
                                 pickerInput(
                                   inputId = "RegionPicker", 
                                   label = NULL, 
                                   choices = countries_region, 
                                   selected = "Asia",
                                   options = list(
                                     `actions-box` = TRUE, 
                                     size = 9,
                                     `selected-text-format` = "count > 3"
                                   ), 
                                   multiple = TRUE
                                 ),
                                 h4("Select Visa Type"),
                                 pickerInput(
                                   inputId = "VisaPicker2", 
                                   label = NULL, 
                                   choices = visa_type, 
                                   selected = visa_type,
                                   options = list(
                                     `actions-box` = TRUE, 
                                     size = 9,
                                     `selected-text-format` = "count > 3"
                                   ), 
                                   multiple = TRUE
                                 ),
                                 
                                 p("Note: 2018 figure is projected number based on 8 months of data"),
                                 
                                 p("* Countries are categorized into regions defined by Department of State. For example, Kosovo, Kyrgyzstan, Tajikistan, Turkmenistan, and Uzbekistan are considered by State as part of Europe instead of Asia.")
                              
                                 
                               ),
                               
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel("Chart", 
                                            plotOutput("region_chart", height="600px")
                                            

                                            
                                   ),
                                   tabPanel("Table",
                                            
                                            dataTableOutput("region_table")
                                            
                                   ))))
                    ),
                    tabPanel("October - May",
                             sidebarLayout(
                               sidebarPanel(
                                 h4("Select Country"),
                                 pickerInput(
                                   inputId = "CountryPickerNM", 
                                   label = NULL, 
                                   choices = countries_list_nm, 
                                   selected = "Afghanistan",
                                   options = list(
                                     `actions-box` = TRUE, 
                                     size = 9,
                                     `selected-text-format` = "count > 3"
                                   ), 
                                   multiple = TRUE
                                 ),
                                 h4("Select Visa Type"),
                                 pickerInput(
                                   inputId = "VisaPicker3", 
                                   label = NULL, 
                                   choices = visa_type_nm, 
                                   selected = visa_type_nm,
                                   options = list(
                                     `actions-box` = TRUE, 
                                     size = 9,
                                     `selected-text-format` = "count > 3"
                                   ), 
                                   multiple = TRUE
                                 )
                                 
                               ),
                               
                               mainPanel(
                                 tabsetPanel(
                                   tabPanel("Chart", 
                                            plotOutput("nm_chart", height="600px")
                                            
                           
                                   ),
                                   tabPanel("Table",
                                            
                                            dataTableOutput("nm_table")
                                            
                                   ))))
                    ),
                    tabPanel("Calculations",
                             
                             
                             
                             mainPanel(
                               h2("Analysis (2018 is projection based on 8 months of data)"),
                               
                               tabsetPanel(
                                 tabPanel("Countries - Percent change", 
                                          dataTableOutput("sum_table1", height="600px")
                                          
                                          
                                          
                                          
                                 ),
                                 
                                 
                                 tabPanel("Visa types - Percent change", 
                                          dataTableOutput("visa_types1", height="600px")
                                          
                                          
                                          
                                          
                                 ),
                                 tabPanel("Visa types - All years", 
                                          dataTableOutput("visa_types3", height="600px")
                                          
                                          
                                          
                                          
                                 )
                               ))
                    )
                  )
                  
                  ))