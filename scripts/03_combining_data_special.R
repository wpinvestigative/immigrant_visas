library(tidyverse)
library(readxl)
library(stringr)

for (i in 1:10) {
  
  consolidated <- read_excel("raw_data/consolidated.xlsx", sheet=i)
  consolidated$area <- ""
  for (x in 1:nrow(consolidated)) {
    if (is.na(consolidated$`Immediate Relatives`[x])) {
      consolidated_area <- consolidated$`Foreign State`[x]
      consolidated$area[x] <- consolidated_area
    }
    consolidated$area[x] <- consolidated_area
  }
  
  
  consolidated_sub <- filter(consolidated, !is.na(`Immediate Relatives`)) %>% 
    filter(!grepl("Region Total", `Foreign State`)) %>% 
    filter(!grepl("Grand Totals", `Foreign State`))
  
  regional <-  filter(consolidated, grepl("Region Total", `Foreign State`) | grepl("Grand Totals", `Foreign State`))
  regional$area <- NULL
  
  if (i == 1) {
    consolidated_df <- consolidated_sub 
    regional_df <- regional
  } else {
    consolidated_df <- rbind(consolidated_df, consolidated_sub)
    regional_df <- rbind(regional_df, regional)
  }
  
  
  
}

consolidated_df$`Foreign State` <- ifelse(consolidated_df$`Foreign State` == "Bahamas, The", "Bahamas", consolidated_df$`Foreign State`)
consolidated_df$`Foreign State` <- ifelse(consolidated_df$`Foreign State` == "Cabo Verde", "Cape Verde", consolidated_df$`Foreign State`)
consolidated_df$`Foreign State` <- ifelse(consolidated_df$`Foreign State` == "Congo, Rep. of the", "Congo, Republic of the", consolidated_df$`Foreign State`)
consolidated_df$`Foreign State` <- ifelse(consolidated_df$`Foreign State` == "Hong Kong S.A.R", "Hong Kong S.A.R.", consolidated_df$`Foreign State`)

for (i in 2:7) {
  sub_cons <- select(consolidated_df, `Foreign State`, i, year) %>% 
    spread(year, 2)
  
  reg_cons <- select(regional_df, `Foreign State`, i, year) %>% 
    spread(year, 2)
  
  write.csv(sub_cons, paste0("output_data/consolidated_subject-", i, ".csv"), row.names=F, na="" )
  write.csv(reg_cons, paste0("output_data/consolidated_regional_subject-", i, ".csv"), row.names=F, na="")
  
  
  
}



# longify

consolidated_df_long <- gather(consolidated_df, 
                               `Immediate Relatives`, `Special Immigrants`, `Family Preference`, 
                               `Employment Preference`, `Diversity Immigrants`, `Total`, key="type", value="total")

regional_df_long <- gather(regional_df, 
                           `Immediate Relatives`, `Special Immigrants`, `Family Preference`, 
                           `Employment Preference`, `Diversity Immigrants`, `Total`, key="type", value="total")

consolidated_df_long <- consolidated_df_long %>% 
  group_by(`Foreign State`, year, area, type) %>% 
  summarize(total=sum(as.numeric(total), na.rm=T))

regional_df_long <- regional_df_long %>% 
  group_by(`Foreign State`, year, type) %>% 
  summarize(total=sum(as.numeric(total), na.rm=T))


saveRDS(consolidated_df_long, "output_data/consolidated_long2.rds")
saveRDS(regional_df_long, "output_data/regional_long2.rds")

nm <- read_excel("raw_data/nov-mar.xlsx", sheet=1)
nm <- mutate(nm, type= case_when(
  grepl("IR", `Visa Class`) ~ "Immediate Relatives",
  grepl("CR", `Visa Class`) ~ "Immediate Relatives",
  grepl("IW", `Visa Class`) ~ "Immediate Relatives",
  grepl("IB", `Visa Class`) ~ "Immediate Relatives",
  grepl("VI", `Visa Class`) ~ "Immediate Relatives",
  grepl("AM", `Visa Class`) ~ "Family Preference",
  grepl("F2", `Visa Class`) ~ "Family Preference",
  grepl("F1", `Visa Class`) ~ "Family Preference",
  grepl("B2", `Visa Class`) ~ "Family Preference",
  grepl("FX", `Visa Class`) ~ "Family Preference",
  grepl("BX", `Visa Class`) ~ "Family Preference",
  grepl("F3", `Visa Class`) ~ "Family Preference",
  grepl("B3", `Visa Class`) ~ "Family Preference",
  grepl("F4", `Visa Class`) ~ "Family Preference",
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
  grepl("C2", `Visa Class`) ~ "Family Preference",
  grepl("IH", `Visa Class`) ~ "Immediate Relatives",
  grepl("SU", `Visa Class`) ~ "Family Preference",
  grepl("BC", `Visa Class`) ~ "Employment Preference",
  grepl("SI", `Visa Class`) ~ "Iraqis/Afghans who helped US abroad",
  grepl("SQ", `Visa Class`) ~ "Iraqis/Afghans who helped US abroad",
  grepl("Grand", `Visa Class`) ~ "Grand Total"
  
)) %>% 
  group_by(`Foreign State of Chargeability or Place of Birth`, date, type) %>% 
  summarize(Issuances=sum(Issuances, na.rm=T))

saveRDS(nm, "output_data/nm2.rds")


## joining to weird categories

countries_only <- select(consolidated_df,Country=`Foreign State`) %>% unique() %>% mutate(Match="yes")

selected_countries <- read_csv("raw_data/specific_categories.csv")
saveRDS(selected_countries, "output_data/categories2.rds")



