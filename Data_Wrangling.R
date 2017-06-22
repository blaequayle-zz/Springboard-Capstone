#Load required library packages
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(rgdal)
library(rgeos)
library(sp)

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

###############################################################

#Repeat process for inner London boroughs dataset

central2016 <- readRDS("central16.rds") #Original data

# Start by taking an overview of the full dataset
glimpse(central2016)

#There are 498,628 observations and 11 variables
#Category and location_type need to be converted to factors

central2016$category <- as.factor(central2016$category)
central2016$location_type <- as.factor(central2016$location_type)

#Due to method used to acquire data, may be duplicates of observations at boundaries of boxes.
#Need to specify a column which has unique identifier for crime.
central2016 <- unique(central2016, by = "id")
#No duplicate crimes are present in the data set.

#Several of the variables contain no useful data for a civilian user.
#The variables context, persistent_id, id and location_subtype will be removed.
#Variable names will be changed to make the dataset clearer.
#The date variable will then be split into a month, and year column.

central2016 <- central2016 %>%
  select(-context, -persistent_id, -id, -location_subtype) %>%
  rename(service = location_type, crime_type = category, date = month)

#Use Lubridate to set month and year individually. First the day (-01) is added so that it is a recognised format.
#Changed to run in reverse order as otherwise the month variable has already changed to a character string.
central2016$year <- year(as.Date(paste(central2016$date,"-01",sep="")))
central2016$month <- month(as.Date(paste(central2016$date,"-01",sep="")))

#The crimes which fall outside of the inner borough area need to be removed
#Set different working directory, load GIS boundaries to subset the 12 inner boroughs
setwd("/Users/Blae/Documents/Work/Data Science/gis_boundaries/ESRI/")
boroughs <- readOGR("statistical-gis-boundaries-london/ESRI", "London_Borough_Excluding_MHW", verbose = FALSE)

#Spatial objects like the boroughs object are made up of a number of different slots
#The key slots being @data (non-geographic attribute data) and @polygons. 
#The data slot can be thought of as an attribute table and the geometry slot is the polgons that make up the physical boundaries.
#@ is used to access specific slots.

head(boroughs@data, 4)
boroughs$NAME
sapply(boroughs@data, class)
nrow(boroughs)

#Names of the boroughs are located in the NAME variable.
central <- boroughs %>% 
  subset(NAME %in% c('Camden','Greenwich','Hackney', 'Hammersmith and Fulham','Islington', 'Kensington and Chelsea','Lambeth','Lewisham', 'Southwark','Tower Hamlets','Wandsworth','Westminster'))
plot(central)

#Convert crime data to a SpatialPointsDataFrame with lat-long projection
coords16 <- SpatialPoints(central2016[,c("longitude","latitude")])
crime16 <- SpatialPointsDataFrame(coords16, central2016)
proj4string(crime16) <- CRS("+init=epsg:4326")

#Transform borough boundaries to geographic coordinates (latitude/longitude)
central <- spTransform(central, CRS("+init=epsg:4326"))
central@proj4string

#Plot the London borough boundaries and the crime data
plot(crime16)
plot(central, border = "blue", add = T)

#Clip the crimes to within the 12 borough boundaries
proj4string(crime16) <- proj4string(central)
crime16 <- crime16[central, ]

#Repeat plotting to check the crimes all fall within boundaries
plot(crime16)
plot(central, border = "blue", add = T)

#Join attributes from the borough boundary polygon layer to the crime date points layer
#Add two new columns to crimes - borough and census code derived from the borough boundary polygon layer.
central_attributes <- over(crime16, central[,c("NAME", "GSS_CODE")])
crime16$borough <- central_attributes$NAME
crime16$census_code <- central_attributes$GSS_CODE
crime16 <- as.data.frame(crime16) 
crime16 <- select(crime16, -longitude.1, -latitude.1)
  
#Create dateframe for monthly crime numbers for 2016
#Abbreviate crime_type in crime_abb for clearer data visualisations
#NB. case_when does not work when post grouping the data so has to be carried out before
monthly.crime.16 <- crime16 %>% 
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

#Create dataframe for seasonal crime numbers
seasonal.crime.16 <- crime16 %>%
  mutate(season = case_when(
    .$month %in% c('12', '1', '2') ~ "Winter",
    .$month %in% c('3', '4', '5') ~ "Spring", 
    .$month %in% c('6', '7', '8') ~ "Summer", 
    .$month %in% c('9', '10', '11') ~ "Autumn",
    TRUE ~ "other")
  ) %>%
  group_by(date, season, crime_type) %>%
  summarize(Count=n())

#Save dataframes for loading into analysis section of project
saveRDS(crime16, "crime16.rds")
saveRDS(monthly.crime.16, "monthlycrime16.rds")
saveRDS(seasonal.crime.16, "seasonalcrime16.rds")







