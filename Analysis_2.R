#Analysis 2 is focussing on using the inner boroughs crime dataset for 2016 (crime16) to create KDE maps
#Load required library packages
library(readr)
library(dplyr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(scales)
library(rgdal)
library(broom)
library(classInt)
library(KernSmooth)
library(RColorBrewer)
library(tidyr)
library(sp)

#Set working directory and read dataset
setwd("/Users/Blae/Documents/Work/Data Science/Capstone_Project/")
crime16 <- readRDS("crime16.rds")
monthlycrime16 <- readRDS("monthlycrime16.rds")
seasonalcrime16 <- readRDS("seasonalcrime16.rds")

#To carry on where analysis 1 left off, KDE maps will be created using 2016 inner boroughs dataset
bike16 <- crime16 %>%
  filter(crime_type == 'bicycle-theft')

#Add jitter in lat long
for(i in 1:length(bike16$latitude)){
  bike16$longitude[i] <- bike16$longitude[i]+runif(1,0.001,0.004)
  bike16$latitude[i] <- bike16$latitude[i]+runif(1,0.001,0.004)}

#KDE map for bike theft
latlong <- select(bike16, longitude, latitude)
kde <- bkde2D(latlong,
              bandwidth=c(.0055, .0048), gridsize = c(75,75))
cl <- contourLines(kde$x1 , kde$x2 , kde$fhat)
levs <- as.factor(sapply(cl, `[[`, "level"))
nlevs <- length(levels(levs))
pgons <- lapply(1:length(cl), function(i)
  Polygons(list(Polygon(cbind(cl[[i]]$x, cl[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(nlevs, NULL)[levs])

#The heat map shows a concentration of crimes in Russell Square (SOAS, Birkbeck and UCL universities), Shoreditch and Dalston

#The process will be repeated for burglary
burglary16 <- crime16 %>%
  filter(crime_type == 'burglary')

#Add jitter in lat long
for(i in 1:length(burglary16$latitude)){
  burglary16$longitude[i] <- burglary16$longitude[i]+runif(1,0.001,0.004)
  burglary16$latitude[i] <- burglary16$latitude[i]+runif(1,0.001,0.004)}

#Heatmap using kernel density estimation for burglary
latlong <- select(burglary16, longitude, latitude)
kde <- bkde2D(latlong,
              bandwidth=c(.0055, .0048), gridsize = c(75,75))
cl <- contourLines(kde$x1 , kde$x2 , kde$fhat)
levs <- as.factor(sapply(cl, `[[`, "level"))
nlevs <- length(levels(levs))
pgons <- lapply(1:length(cl), function(i)
  Polygons(list(Polygon(cbind(cl[[i]]$x, cl[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(nlevs, NULL)[levs])

#The KDE map shows a high concentration of burglaries in Fitzrovia and into Soho,
#with Brixton, Paddington and Shoreditch also showing more minor hotspots

#Repeat the KDE map process for theft from the person
TFTP16 <- crime16 %>%
  filter(crime_type == 'theft-from-the-person')

#Add jitter in lat long
for(i in 1:length(TFTP16$latitude)){
  TFTP16$longitude[i] <- TFTP16$longitude[i]+runif(1,0.001,0.004)
  TFTP16$latitude[i] <- TFTP16$latitude[i]+runif(1,0.001,0.004)}

#KDE map for bike theft
latlong <- select(TFTP16, longitude, latitude)
kde <- bkde2D(latlong,
              bandwidth=c(.0055, .0048), gridsize = c(75,75))
cl <- contourLines(kde$x1 , kde$x2 , kde$fhat)
levs <- as.factor(sapply(cl, `[[`, "level"))
nlevs <- length(levels(levs))
pgons <- lapply(1:length(cl), function(i)
  Polygons(list(Polygon(cbind(cl[[i]]$x, cl[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)

leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(nlevs, NULL)[levs])

#The KDE map shows a very concentrated hotspot around Soho and Covent Garden, and a smaller one over Shoreditch
#These three areas contain the high density of nightclubs and bars in London

#Returning to general 2016 analysis, it would be preferable to not use absolute crime rates but to normalise by borough population
employment <- read_csv("employment_london.csv")
employment <- employment %>% 
  as.data.frame() %>%
  na.omit()

#Frequency of types of crime by borough
borough.rate <- crime16 %>%
  group_by(borough) %>%
  summarize(Count=n())

#Normalise data so "norm" relfects the crime reports per 100 people
borough.rate$pop <- employment$Population
borough.rate <- borough.rate %>%
  mutate(norm = Count/pop*100) %>%
  arrange(desc(norm))
View(borough.rate)
saveRDS(borough.rate, "modelinput.rds")

#Overall, the borough with fewest crimes per 100 people in 2016 was Wandsworth with 9.66.
#Westminster had the highest crime rate with 28.7 crimes per 100 people. This is likely to 
#be highly skewed due to the number of tourists who visit the area and are not accounted
#for in the normalisation

#Let's look at specific crimes per borough, without the normalisation
crime.rate.by.borough <- crime16 %>%
  group_by(borough, crime_type) %>%
  summarize(Count=n())

ggplot(crime.rate.by.borough) +
  geom_bar(stat="identity",aes(x = borough, y = Count,fill = crime_type)) +
  theme(panel.grid = element_blank(), axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(lineheight=.8, face="bold")) +
  ggtitle("Crime by Borough") +
  ylab("Crime occurences") +
  xlab("Borough") 

#There is significant variation between crime in the boroughs with Westminster having the
#most recorded crimes particularly for other-theft and theft-from-the-person, and
#Kensingston & Chelsea the least. 

#The crimes need to be normalised to account for the population in each borough
boroughpop <- select(borough.rate, borough, pop)
crime.borough.norm <- left_join(crime.rate.by.borough, boroughpop) %>%
  mutate(norm = Count/pop*100) %>% arrange(desc(norm))

borough.col <- c("Camden" = "red3", "Greenwich" = "orange", "Hackney" = "yellow", "Hammersmith and Fulham" = "limegreen", "Islington" = "green", "Kensington and Chelsea" = "royalblue", "Lambeth" = "lightblue", "Lewisham" = "aquamarine", "Southwark" = "lightpink", "Tower Hamlets" = "violet", "Wandsworth" = "deeppink", "Westminster" = "darkgrey")

ggplot(crime.borough.norm) +
  geom_bar(stat="identity",aes(x = borough, y = norm, fill = crime_type)) +
  theme(panel.grid = element_blank(), axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(lineheight=.8, face="bold")) +
  ggtitle("Types of Crime by Borough (Normalised)") +
  ylab("Crime occurence per 100 people") +
  xlab("Borough") 

ggplot(crime.borough.norm, aes(x = crime_type, y = norm)) +
  geom_point(aes(col = borough)) +
  geom_line(aes(col = borough, group = borough)) +
  xlab("Crime type") +
  ylab("Crime occurences per 100 people") +
  ggtitle("Types of Crime by Borough (Normalised)") +
  scale_colour_manual(values = borough.col) +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black"), axis.text.x=element_text(angle=60, hjust=1)) +
  ggsave("crimetype.norm.png")

#Relative to its population, Wesminster has even more crimes per 100 residents. 
#This does take the large number of tourists who visit the borough every day into account.
#This is three times the number of crimes recorded in Wandsworth per 100 residents, which is consistently low across all crimes.
#other-theft, shoplifting, theft-from-the-person and violent-crime are all elevated above the main trend.

#Using geom_point and geom_line we can analyse the monthly variation for 2016
crime.col <- c("ASB" = "sienna", "BT" = "red3", "BU" = "orange", "CDA" = "yellow", "DR" = "lightgreen", "OC" = "limegreen", "OT" = "green", "PDW" = "olivedrab", "PO" = "royalblue", "PoW" = "lightblue", "RO" = "aquamarine", "SL" = "lightpink", "TP" = "violet", "VEC" = "deeppink", "VIC" = "darkgrey")

ggplot(monthlycrime16, aes(x = month, y = Count)) +
  geom_point(aes(col = crime_abb)) +
  geom_line(aes(col = crime_abb)) +
  xlab("Month") +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values = crime.col) +
  ggtitle("Monthly Variation in Crime Type (2016)")

#The anti-social behaviour peak in summer is very clear still across the inner borough dataset.

#Heatmap by month - seasonal analysis
ggplot(monthlycrime16,aes(x=crime_abb,y=month,fill=Count))+
  geom_tile(aes(fill=Count))
#Violent crime and vehicle crime are also more prevalent in May-July.

