#Analysis 1 is focussing on invesigating the original dataset spanning 2011-2017

#Load required library packages
library(readr)
library(dplyr)
library(tidyr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(scales)
library(rgdal)
library(broom)
library(classInt)
library(KernSmooth)
library(RColorBrewer)
library(sp)

#Read dataset
setwd("/Users/Blae/Documents/Work/Data Science/Capstone_Project/")
crime11to17 <- readRDS("crime11to17.rds")
monthly.crime <- readRDS("monthlycrime.rds")
annual.crime <- readRDS("annualcrime.rds")
seasonal.crime <- readRDS("seasonalcrime.rds")

#Look at a broad overview of the dataset.

summary(crime11to17)
table(crime11to17$crime_type)

#The most prevalent street crime in London, out of 15 categories, is anti-social behaviour

asb.count <- count(crime11to17, crime_type == "anti-social-behaviour")
total.count <- nrow(crime11to17)
print((asb.count[2,2]/total.count)*100)

#It makes up 23.1% of recorded crime

#The majority of the records are from the Police Force but a small portion is submitted by the British Transport Police who operate on the railways
service.count <- count(crime11to17, service == "BTP")
print(service.count[2,2]/total.count)
#These records account for only 4.3%

#Investigation of the annual variation in street crimes by plotting a bar chart and scatter graphs
total.year <- crime11to17 %>% 
  group_by(year) %>%
  summarize(Count=n()) %>%
  filter(year != 2017) %>%
  as.data.frame()

ggplot(total.year, aes(x = year, y = Count)) +
  geom_col(col = "blue", fill = "lightgrey") + 
  ggtitle("Annual variation in street crime occurence (2011-2016)") +
  xlab("Year") +
  ylab("Number of crimes")
ggsave("total.year.png")

ggplot(annual.crime) +
  geom_bar(stat="identity",aes(x = year, y = Count,fill = crime_abb)) +
  theme(panel.grid = element_blank()) +
  ggtitle("Annual variation in crime type") +
  xlab("Year") +
  ylab("Number of crimes")
ggsave("annual.crime.png")

#Percentage decrease between 2012 and 2013
((total.year[2,2]-total.year[3,2])/total.year[2,2])*100

ggplot(annual.crime, aes(x = year, y = Count)) +
  geom_point(aes(col = crime_abb)) +
  geom_line(aes(col = crime_abb)) +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black"))
ggsave("annual.line.png")

#Three categories have shown a strong decrease between 2011 and 2016 - other-crime, other-theft and anti-social-behaviour 
#Background of continuously decreasing reported street crimes 
#The anonymously large drop between 2012 and 2013 may suggest a change in recording/reporting

#Investigation of the monthly to seasonal variation in street crimes

#First, need to set up a date column with the correct format using Lubridate
monthly.crime <- monthly.crime %>% 
  unite(date, month, year, sep = "-", remove = FALSE)
monthly.crime$date <- paste0("01-", monthly.crime$date) 
monthly.crime$date <- dmy(monthly.crime$date)

#The colours are difficult to make out so create a colour scale which is more effective.
#Vector also created to use for vertical markers on plots, indicating start of year.

crime.col <- c("ASB" = "sienna", "BT" = "red3", "BU" = "orange", "CDA" = "yellow", "DR" = "lightgreen", "OC" = "limegreen", "OT" = "green", "PDW" = "olivedrab", "PO" = "royalblue", "PoW" = "lightblue", "RO" = "aquamarine", "SL" = "lightpink", "TP" = "violet", "VEC" = "deeppink", "VIC" = "grey")
season.cols <- c("Summer" = "orange", "Autumn" = "darkgreen", "Winter" = "darkblue", "Spring" = "purple")
annual.line <- c("2012-01-01", "2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01")

TDates <- c("2011-06-27", "2012-08-19", "2013-07-22", "2014-07-18", "2015-07-01", "2016-07-19")
HTemp <- c(30, 30, 33, 30, 35, 33)
hottest.day <- data.frame(x = TDates, y = HTemp)

ggplot(monthly.crime, aes(x = date,  y = Count, fill = crime_abb)) +
  geom_area() +
  scale_fill_manual(values = crime.col) +
  ggtitle("Annual variation in street crime occurence (2011-2017)") +
  xlab("Year") +
  ylab("Number of crimes")
ggsave("annual.png")

ggplot(monthly.crime) +
  geom_bar(stat="identity",aes(x = date, y = Count,fill = crime_abb)) +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Date") +
  ylab("Number of crimes") +
  scale_fill_manual(values = crime.col)

#Heatmap can be used to identify seasonal trends
ggplot(monthly.crime,aes(x=crime_abb,y=month,fill=Count))+
  geom_tile(aes(fill=Count)) +
  ggtitle("Crime Heatmap - Seasonal Trends")

#Anti-social behaviour appears to display peaks each summer, but due to the scale used it is difficult to tell.
#Break out anti-social behaviour to analyse peaks.

seasonal.crime$date <- paste0("01-", seasonal.crime$date) 
seasonal.crime$date <- dmy(seasonal.crime$date)
ASB.season <- filter(seasonal.crime, crime_type == "anti-social-behaviour")

ggplot(ASB.season, aes(x = date, y = Count)) +
  geom_line(aes(group = crime_type, col = season)) + 
  scale_colour_manual(values = season.cols) +
  scale_x_date(date_breaks = "6 month", date_labels = "%m-%y") +
  ylab("Number of crimes") +
  xlab("Date") +
  ggtitle("Anti-Social Behaviour 2011 - 2017") +
  theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(lineheight=.8, face="bold")) +
  geom_vline(xintercept = as.numeric(as.Date(TDates)), linetype=2, col = "red")
ggsave("asb.png")

#There has been a clear decrease in anti-social behaviour over time, with the exception of 2016 which showed a significant increase.
#Clear peaks are visible in the summer (orange segments), often falling in August (school summer holidays).
#The peaks frequently coincide very closely to the hottest day of the year, with the exception of 2014.
#Minimum occurences are consistently just after New Year.

#Repeat this seasonal analysis for bicycle theft

BT.season <- filter(seasonal.crime, crime_type == "bicycle-theft")

ggplot(BT.season, aes(x = date, y = Count)) +
  geom_line(aes(group = crime_type, col = season)) + 
  scale_colour_manual(values = season.cols) +
  scale_x_date(date_breaks = "6 month", date_labels = "%m-%y") +
  ylab("Number of crimes") +
  xlab("Date") +
  ggtitle("Bicycle Theft Time Series") +
  theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(lineheight=.8, face="bold")) +
  geom_vline(xintercept = as.numeric(as.Date(TDates)), linetype=2, col = "red")
ggsave("bicycle-theft.png")

#Repeat this seasonal analysis for burglary

BU.season <- filter(seasonal.crime, crime_type == "burglary")

ggplot(BU.season, aes(x = date, y = Count)) +
  geom_line(aes(group = crime_type, col = season)) + 
  scale_colour_manual(values = season.cols) +
  scale_x_date(date_breaks = "6 month", date_labels = "%m-%y") +
  ylab("Number of crimes") +
  xlab("Date") +
  ggtitle("Burglary Time Series") +
  theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(lineheight=.8, face="bold")) +
  geom_vline(xintercept = as.numeric(as.Date(TDates)), linetype=2, col = "red")
ggsave("burglary.png")
  
#There is less of a pattern visible in burglary occurences, though there is frequently a peak in the run up to Christmas.

#Use Leaflet to visualise location of specific crime.
#Reduced dataset required due to processing capability, limited to around three months of data.


VEC.2016 <- crime11to17 %>%
filter(year == 2016, crime_type == 'vehicle-crime')

VEC.2016 %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude)

#Just displaying the crime locations not particularly useful, either a KDE map showing areas
#of high density street crime or a chloropleth map giving an indication per borough would be useful.
