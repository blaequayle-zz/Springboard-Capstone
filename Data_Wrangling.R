#Load required library packages
library(readr)
library(dplyr)
library(tidyr)

# Start by taking an overview of the full dataset
glimpse(crime11to17)

#There are 498,628 observations and 11 variables
#Category and location_type need to be converted to factors

crime11to17$category <- as.factor(crime11to17$category)
crime11to17$location_type <- as.factor(crime11to17$location_type)

#Several of the variables contain no useful data for a civilian user.
#The variables context, persistent_id, id and location_subtype will be removed.
#Variable names will be changed to make the dataset clearer, including changing month to date.
#The date variable will then be split into a month, and year column.

crime.full.narrow <- crime11to17 %>%
  select(-context, -persistent_id, -id, -location_subtype) %>%
  rename(date = month, service = location_type, crime_type = category) %>%
  separate('date', c("year", "month"), sep = "-", remove = FALSE)

#Identifying missing values
sapply(crime.full.narrow, function(x) sum(is.na(x)))

#Only the outcome_status_category and outcome_status _data contain NA values. 
#Set "status update unavailable" under outcome_status_category to NA as it provides no further information.

is.na(crime.full.narrow) <- crime.full.narrow == "Status update unavailable"

#How do I change the equivalent dates to NA?

if (crime.full.narrow$outcome_status_category = "Status update unavailable") {
  
}

#Create df which contains annual crime numbers for top five street crimes 2011-2016

#anti-social behaviour 
ASB <- crime.full.narrow %>% 
  filter(crime_type == 'anti-social-behaviour') %>%
  group_by(year) %>%
  count(year, sort = TRUE)

ASB <- ASB %>%
  filter(year < 2017) %>%
  mutate(crime_type = "ASB")

glimpse(ASB)

#other-theft 
OT <- crime.full.narrow %>% 
  filter(crime_type == 'other-theft') %>%
  group_by(year) %>%
  count(year, sort = TRUE)

OT <- OT %>%
  filter(year < 2017) %>%
  mutate(crime_type = "OT")

glimpse(OT)

#violent-crime
VC <- crime.full.narrow %>% 
  filter(crime_type == 'violent-crime') %>%
  group_by(year) %>%
  count(year, sort = TRUE)

VC <- VC %>%
  filter(year < 2017) %>%
  mutate(crime_type = "VC")

glimpse(VC)

#other-crime
OC <- crime.full.narrow %>% 
  filter(crime_type == 'other-crime') %>%
  group_by(year) %>%
  count(year, sort = TRUE)

OC <- OC %>%
  filter(year < 2017) %>%
  mutate(crime_type = "OC")

glimpse(OC)

#shoplifting
SL <- crime.full.narrow %>% 
  filter(crime_type == 'shoplifting') %>%
  group_by(year) %>%
  count(year, sort = TRUE)

SL <- SL %>%
  filter(year < 2017) %>%
  mutate(crime_type = "SL")

glimpse(SL)

#Combine into one dataframe and change crime_type from character to factor
annual.crime <- data.frame(bind_rows(OC, OT, SL, ASB, VC))
annual.crime$crime_type <- as.factor(annual.crime$crime_type)
glimpse(annual.crime)

#Repeat process to create df which contains month by month crime numbers for top five street crimes 2011-2016
ASB.M <- crime.full.narrow %>% 
  filter(crime_type == 'anti-social-behaviour') %>%
  group_by(date) %>%
  count(date, sort = TRUE)

ASB.M <- ASB.M %>%
  mutate(crime_type = "ASB")

glimpse(ASB.M)

#other-theft 
OT.M <- crime.full.narrow %>% 
  filter(crime_type == 'other-theft') %>%
  group_by(date) %>%
  count(date, sort = TRUE)

OT.M <- OT.M %>%
  mutate(crime_type = "OT")

glimpse(OT.M)

#violent-crime
VC.M <- crime.full.narrow %>% 
  filter(crime_type == 'violent-crime') %>%
  group_by(date) %>%
  count(date, sort = TRUE)

VC.M <- VC.M %>%
  mutate(crime_type = "VC")

glimpse(VC.M)

#other-crime
OC.M <- crime.full.narrow %>% 
  filter(crime_type == 'other-crime') %>%
  group_by(date) %>%
  count(date, sort = TRUE)

OC.M <- OC.M %>%
  mutate(crime_type = "OC")

glimpse(OC.M)

#shoplifting
SL.M <- crime.full.narrow %>% 
  filter(crime_type == 'shoplifting') %>%
  group_by(date) %>%
  count(date, sort = TRUE)

SL.M <- SL.M %>%
  mutate(crime_type = "SL")

glimpse(SL.M)

#Combine into one dataframe and change crime_type from character to factor

monthly.crime <- data.frame(bind_rows(OC.M, OT.M, SL.M, ASB.M, VC.M))
monthly.crime$crime_type <- as.factor(monthly.crime$crime_type)
format(Sys.Date(), "%Y-%m")
monthly.crime$date <- as.Date(monthly.crime$date)
glimpse(monthly.crime)

#Repeat process to create df so that seasonal trends can be analysed.
#December, January, February as Winter
#March, April, May as Spring
#June, July, August as Summer
#September, October, November as Autumn

