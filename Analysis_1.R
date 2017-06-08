#Load required library packages
library(readr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(scales)

#Initial exploration of data

summary(crime.full.narrow)
table(crime.full.narrow$crime_type)

#15 different categories of crime

asb.count <- count(crime.full.narrow, crime_type == "anti-social-behaviour")
total.count <- nrow(crime.full.narrow)
print(asb.count[2,2]/total.count)

#Anti-social behaviour accounts for 24.8% of observations.

service.count <- count(crime.full.narrow, service == "BTP")
print(service.count[2,2]/total.count)

#British Transport Police observations account for only 3.9%.

crime.by.year <- as.data.frame(tbl_df(table(crime.full.narrow$year)))
crime.by.year <- rename(crime.by.year, year = Var1)
#2017 is only reporting three months of the year, remove 2017 from table
crime.by.year <- filter(crime.by.year, year < 2017)

ggplot(crime.by.year, aes(x = year, y = n)) +
  geom_point()

#Plot bar plot as discrete x variabkle

ggplot(crime.by.year, aes(x = year, fill = n)) +
  geom_bar()


#Total recorded crimes decreased between 2011 and 2015, beginning to increase again in 2016.

location.map <- crime2017full %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude)

crime2017burg <- filter(crime2017full, category == "burglary")

location.map.burglaries <- crime2017burg %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude)

print(location.map.burglaries)

#Plotting annual crime numbers for 5 most prevalent crimes

ann.cri.plot <- ggplot(annual.crime, aes(x = year, y = n, col = crime_type)) +
  geom_line(aes(group = crime_type)) +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_discrete(name = "Year")

print(ann.cri.plot)

ann.cri.plot.2 <- ggplot(annual.crime, aes(x = year, fill = n)) +
  geom_bar(position = "dodge")

print(ann.cri.plot.2)

#Plotting monthly crime numbers for 5 most prevalent crimes
mon.cri.plot <- ggplot(monthly.crime, aes(x = date, y = n, col = crime_type)) +
  geom_line(aes(group = crime_type)) +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Month") +
  ylab("Count") +
  scale_x_date(breaks=data_breaks("6 months"), labels=date_format ("%Y-%m"))

print(mon.cri.plot)

#Plotting seasonal crime numbers for 5 most prevalent crimes