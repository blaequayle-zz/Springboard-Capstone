#Load required library packages
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

#Load dataset from data acquisition
setwd("/Users/Blae/Documents/Work/Data Science/Capstone_Project/")
crime11to17 <- readRDS("crime11to17.rds")

# Start by taking an overview of the full dataset
glimpse(crime11to17)

#There are 498,628 observations and 11 variables
#Category and location_type need to be converted to factors

crime11to17$category <- as.factor(crime11to17$category)
crime11to17$location_type <- as.factor(crime11to17$location_type)

#Several of the variables contain no useful data for a civilian user.
#The variables context, persistent_id, id and location_subtype will be removed.
#Variable names will be changed to make the dataset clearer.
#The date variable will then be split into a month, and year column.

crime11to17 <- crime11to17 %>%
  select(-context, -persistent_id, -id, -location_subtype) %>%
  rename(service = location_type, crime_type = category)
  
#Identifying missing values
sapply(crime11to17, function(x) sum(is.na(x)))

#Only the outcome_status_category and outcome_status_date contain NA values, and there is currently no plan to use these during analysis.

#Use Lubridate to set month and year individually. First the day (-01) is added so that it is a recognised format.
#Changed to run in reverse order as otherwise the month variable has already changed to a character string.
crime11to17$year <- year(as.Date(paste(crime11to17$month,"-01",sep="")))
crime11to17$month <- month(as.Date(paste(crime11to17$month,"-01",sep="")), label=T)

#Create dateframe for monthly crime numbers for 2011-2017
#Abbreviate crime_type in crime_abb for clearer data visualisations
#NB. case_when does not work when post grouping the data so has to be carried out before
monthly.crime <- crime11to17 %>% 
  mutate(crime_abb = case_when(
      .$crime_type == 'anti-social-behaviour' ~ "ASB",
      .$crime_type == 'bicycle-theft' ~ "BT",
      .$crime_type == 'burglary' ~ "BU",
      .$crime_type == 'criminal-damage-arson' ~ "CDA",
      .$crime_type == 'drugs' ~ "DR",
      .$crime_type == 'other-crime' ~ "OC",
      .$crime_type == 'other-theft' ~ "OT",
      .$crime_type == 'possession-of-weapons' ~ "PoW",
      .$crime_type == 'public-disorder-weapons' ~ "PDW",
      .$crime_type == 'public-order' ~ "PO",
      .$crime_type == 'robbery' ~ "RO",
      .$crime_type == 'shoplifting' ~ "SL",
      .$crime_type == 'theft-from-the-person' ~ "TP",
      .$crime_type == 'vehicle-crime' ~ "VEC",
      .$crime_type == 'violent-crime' ~ "VIC",
      TRUE ~ "other")
      )  %>%
group_by(year, month, crime_abb) %>%
  summarize(Count=n())

#Create dateframe for annual crime numbers for 2011-2016, removed 2017 as only three months
#Abbreviate crime_type in crime_abb for clearer data visualisations
annual.crime <- crime11to17 %>% 
  mutate(crime_abb = case_when(
    .$crime_type == 'anti-social-behaviour' ~ 'ASB',
    .$crime_type == 'bicycle-theft' ~ 'BT',
    .$crime_type == 'burglary' ~ 'BU',
    .$crime_type == 'criminal-damage-arson' ~ 'CDA',
    .$crime_type == 'drugs' ~ 'DR',
    .$crime_type == 'other-crime' ~ 'OC',
    .$crime_type == 'other-theft' ~ 'OT',
    .$crime_type == 'possession-of-weapons' ~ 'PoW',
    .$crime_type == 'public-disorder-weapons' ~ 'PDW',
    .$crime_type == 'public-order' ~ 'PO',
    .$crime_type == 'robbery' ~ 'RO',
    .$crime_type == 'shoplifting' ~ 'SL',
    .$crime_type == 'theft-from-the-person' ~ 'TP',
    .$crime_type == 'vehicle-crime' ~ 'VEC',
    .$crime_type == 'violent-crime' ~ 'VIC',
    TRUE ~ "other")
  ) %>%
  group_by(year, crime_abb) %>%
  filter(year < 2017) %>%
  summarize(Count=n())
  
#Create dataframe for seasonal crime numbers, with March 2017 removed
seasonal.crime <- crime11to17 %>%
    mutate(season = case_when(
    .$month %in% c('Dec', 'Jan', 'Feb') ~ "Winter",
    .$month %in% c('Mar', 'Apr', 'May') ~ "Spring", 
    .$month %in% c('Jun', 'Jul', 'Aug') ~ "Summer", 
    .$month %in% c('Sep', 'Oct', 'Nov') ~ "Autumn",
    TRUE ~ "other")
    ) %>%
  unite(date, month, year, sep = "-") %>%
  filter(date != "Mar-2017") %>%
  group_by(date, season, crime_type) %>%
  summarize(Count=n())

#Save dataframes for loading into analysis section of project
saveRDS(crime11to17, "crime11to17.rds")
saveRDS(monthly.crime, "monthlycrime.rds")
saveRDS(annual.crime, "annualcrime.rds")
saveRDS(seasonal.crime, "seasonalcrime.rds")








