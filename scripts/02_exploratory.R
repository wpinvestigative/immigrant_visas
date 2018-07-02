# A script for exploratory purposes
# Cleaned up data, and organized a list of questions that needed answers
# Transformed data 
# Code was repurposed to be used in the shiny app

library(tidyverse)

round = function(x) ifelse(x>0, trunc(x+0.5), trunc(x-0.5));

nm_countries <- unique(nm$`Foreign State of Chargeability or Place of Birth`)
ye_countries <- unique(countries$`Foreign State`)

countries_regions <- select(countries, `Foreign State`, area) %>% ungroup() %>% unique
countries_regions$year <- NULL
countries_regions <- unique(countries_regions)

nmc <- data.frame(nm_countries)
colnames(nmc) <- "Country"
nmc$type <- "Monthly"

yec <- data.frame(ye_countries)
colnames(yec) <- "Country"

cnames <- full_join(nmc, yec)


nm_projected <- nm %>% 
  mutate(type=gsub("Iraqis/Afghans who helped US abroad", "Special Immigrants", type)) %>% 
  group_by(`Foreign State of Chargeability or Place of Birth`, date, type) %>% 
  summarize(Issuances=sum(Issuances, na.rm=T)) %>% 
  group_by(`Foreign State of Chargeability or Place of Birth`, type) %>% 
  ## if basing on 5 months of available data
  summarize(total=round(sum(Issuances)/5*12)) %>%
  dplyr::rename(`Foreign State`=`Foreign State of Chargeability or Place of Birth`) %>% 
  left_join(countries_regions) %>% 
  mutate(year=2018) %>% 
  select(`Foreign State`, year, area, type, total)
  ## if basing on 2018 months only
  #filter(grepl("2018", as.character(date)) %>% 
  #summarize(`2018 (Projected)`=sum(Issuances)/3*12) %>% 
  

countries <- rbind(countries, nm_projected) %>% ungroup()

nm_projected2 <- select(nm_projected, `Foreign State`, year, area, type, total) %>% 
  summarize(what=sum(total, na.rm=T)) %>% 
  mutate(type="Total", year=2018) %>%
  left_join(countries_regions) %>% 
  select(`Foreign State`, year, area, type, total=what)

countries <- rbind(countries, nm_projected2)

# Percent change 2009 - 2017
# Percent change 2009 - 2018 (Projected)
# Percent change 2009 - 2016 (Obama)
# Percent change 2017 - 2018 (Projected)
# Average annual Obama
# 2017 for Trump
# Average annual Trump (with projected)

countries_wide <- spread(countries, year, total) %>% 
  
  mutate(`Percent change '09 - '16'` =round((`2016`-`2009`)/`2009`*100),
         `Percent change '09 - '17'` =round((`2017`-`2009`)/`2009`*100),
         `Percent change '09 - '18'` =round((`2018`-`2009`)/`2009`*100),
         `Percent change '16 - '17'` =round((`2017`-`2016`)/`2009`*100),
         `Percent change '17 - '18'` =round((`2018`-`2017`)/`2009`*100),
    `Obama average` = round((`2008`+`2009`+`2010`+`2011`+`2012`+`2013`+`2014`+`2015`+`2016`)/9),
    `Trump 2017`=`2017`,
    `Trump projected average`=round((`2017`+`2018`)/2),
    `Percent change O - T`=round((`Trump 2017`-`Obama average`)/`Obama average`*100),
    `Percent change O - T (p)`=round((`Trump projected average`-`Obama average`)/`Obama average`*100))



