# loading packages

library(tidyverse)
library(readxl)
library(stringr)

# Bringing in annual visa data
# Compiled from https://travel.state.gov/content/travel/en/legal/visa-law0/visa-statistics/annual-reports.html
# Each sheet on consolidated.xsx is a different year of data scraped from PDFs

# There are 10 years of data and this loop pulls each one and builds a data frame called consolidated

for (i in 1:10) {

  consolidated <- read_excel("raw_data/consolidated.xlsx", sheet=i)
  # Tidying up the region designation by taking it from the blank row and adding it as a variable
  consolidated$area <- ""
  for (x in 1:nrow(consolidated)) {
    if (is.na(consolidated$`Immediate Relatives`[x])) {
      consolidated_area <- consolidated$`Foreign State`[x]
      consolidated$area[x] <- consolidated_area
    }
  consolidated$area[x] <- consolidated_area
  }

  # Don't need Region or Grand Totals
  consolidated_sub <- filter(consolidated, !is.na(`Immediate Relatives`)) %>% 
    filter(!grepl("Region Total", `Foreign State`)) %>% 
    filter(!grepl("Grand Totals", `Foreign State`))
  
  regional <-  filter(consolidated, grepl("Region Total", `Foreign State`) | grepl("Grand Totals", `Foreign State`))
  regional$area <- NULL
  
  # Building a dataframe from the loop
  if (i == 1) {
    consolidated_df <- consolidated_sub 
    regional_df <- regional
  } else {
    consolidated_df <- rbind(consolidated_df, consolidated_sub)
    regional_df <- rbind(regional_df, regional)
  }
  
  
  
}

# Deaing with inconsistent country names from year to year

consolidated_df$`Foreign State` <- ifelse(consolidated_df$`Foreign State` == "Bahamas, The", "Bahamas", consolidated_df$`Foreign State`)
consolidated_df$`Foreign State` <- ifelse(consolidated_df$`Foreign State` == "Cabo Verde", "Cape Verde", consolidated_df$`Foreign State`)
consolidated_df$`Foreign State` <- ifelse(consolidated_df$`Foreign State` == "Congo, Rep. of the", "Congo, Republic of the", consolidated_df$`Foreign State`)
consolidated_df$`Foreign State` <- ifelse(consolidated_df$`Foreign State` == "Hong Kong S.A.R", "Hong Kong S.A.R.", consolidated_df$`Foreign State`)

# Generating spreadsheets based on the consoidated data
for (i in 2:7) {
  
  # spreadsheet based on subject and country over time
  sub_cons <- select(consolidated_df, `Foreign State`, i, year) %>% 
    spread(year, 2)
  
  # spreadsheet based on subject and area over time
  reg_cons <- select(regional_df, `Foreign State`, i, year) %>% 
    spread(year, 2)

  write.csv(sub_cons, paste0("output_data/consolidated_subject-", i, ".csv"), row.names=F, na="" )
  write.csv(reg_cons, paste0("output_data/consolidated_regional_subject-", i, ".csv"), row.names=F, na="")
  
  
  
}

# Creating a long, tidy version of the countries and regions data

consolidated_df_long <- gather(consolidated_df, 
                               `Immediate Relatives`, `Special Immigrants`, `Family Preference`, 
                               `Employment Preference`, `Diversity Immigrants`, `Total`, key="type", value="total")

regional_df_long <- gather(regional_df, 
                               `Immediate Relatives`, `Special Immigrants`, `Family Preference`, 
                               `Employment Preference`, `Diversity Immigrants`, `Total`, key="type", value="total")

# Combining Family Preference and Immediate Relatives into Family Visas
consolidated_df_long <- mutate(consolidated_df_long, 
  type=str_replace(type,"Family Preference", "Family Visas"),
  type=str_replace(type, "Immediate Relatives",  "Family Visas")) %>% 
  group_by(`Foreign State`, year, area, type) %>% 
  summarize(total=sum(as.numeric(total), na.rm=T))

regional_df_long <- mutate(regional_df_long, 
                               type=str_replace(type,"Family Preference", "Family Visas"),
                               type=str_replace(type, "Immediate Relatives",  "Family Visas")) %>% 
  group_by(`Foreign State`, year, type) %>% 
  summarize(total=sum(as.numeric(total), na.rm=T))

# Saving the data into the output folder
saveRDS(consolidated_df_long, "output_data/consolidated_long.rds")
saveRDS(regional_df_long, "output_data/regional_long.rds")

nm <- read_excel("raw_data/nov-mar.xlsx", sheet=1)
nm <- mutate(nm, type= case_when(
  grepl("IR", `Visa Class`) ~ "Family Visas",
  grepl("CR", `Visa Class`) ~ "Family Visas",
  grepl("IW", `Visa Class`) ~ "Family Visas",
  grepl("IB", `Visa Class`) ~ "Family Visas",
  grepl("VI", `Visa Class`) ~ "Family Visas",
  grepl("AM", `Visa Class`) ~ "Family Visas",
  grepl("F2", `Visa Class`) ~ "Family Visas",
  grepl("F1", `Visa Class`) ~ "Family Visas",
  grepl("B2", `Visa Class`) ~ "Family Visas",
  grepl("FX", `Visa Class`) ~ "Family Visas",
  grepl("BX", `Visa Class`) ~ "Family Visas",
  grepl("F3", `Visa Class`) ~ "Family Visas",
  grepl("B3", `Visa Class`) ~ "Family Visas",
  grepl("F4", `Visa Class`) ~ "Family Visas",
  grepl("SB", `Visa Class`) ~ "Special Immigrants",
  grepl("SC", `Visa Class`) ~ "Special Immigrants",
  grepl("E1", `Visa Class`) ~ "Employment Preference",
  grepl("E2", `Visa Class`) ~ "Employment Preference",
  grepl("E3", `Visa Class`) ~ "Employment Preference",
  grepl("EW", `Visa Class`) ~ "Employment Preference",
  grepl("SD", `Visa Class`) ~ "Employment Preference",
  grepl("SE", `Visa Class`) ~ "Employment Preference",
  grepl("SG", `Visa Class`) ~ "Employment Preference",
  grepl("SH", `Visa Class`) ~ "Employment Preference",
  grepl("SJ", `Visa Class`) ~ "Employment Preference",
  grepl("SK", `Visa Class`) ~ "Employment Preference",
  grepl("SL", `Visa Class`) ~ "Employment Preference",
  grepl("SM", `Visa Class`) ~ "Employment Preference",
  grepl("SR", `Visa Class`) ~ "Employment Preference",
  grepl("C5", `Visa Class`) ~ "Employment Preference",
  grepl("T5", `Visa Class`) ~ "Employment Preference",
  grepl("R5", `Visa Class`) ~ "Employment Preference",
  grepl("I5", `Visa Class`) ~ "Employment Preference",
  grepl("DV", `Visa Class`) ~ "Diversity Immigrants",
  grepl("C2", `Visa Class`) ~ "Family Visas",
  grepl("IH", `Visa Class`) ~ "Family Visas",
  grepl("SU", `Visa Class`) ~ "Family Visas",
  grepl("BC", `Visa Class`) ~ "Employment Preference",
  grepl("SI", `Visa Class`) ~ "Iraqis/Afghans who helped US abroad",
  grepl("SQ", `Visa Class`) ~ "Iraqis/Afghans who helped US abroad",
  grepl("Grand", `Visa Class`) ~ "Grand Total"
  
)) %>% 
  group_by(`Foreign State of Chargeability or Place of Birth`, date, type) %>% 
  summarize(Issuances=sum(Issuances, na.rm=T))

saveRDS(nm, "output_data/nm.rds")


## joining to weird categories

countries_only <- select(consolidated_df,Country=`Foreign State`) %>% unique() %>% mutate(Match="yes")

selected_countries <- read_csv("raw_data/specific_categories.csv")
saveRDS(selected_countries, "output_data/categories.rds")



