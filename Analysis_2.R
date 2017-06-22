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

#Crime rates need to be normalised by borough population
employment <- read_csv("employment_london.csv")
employment <- employment %>% 
  as.data.frame() %>%
  na.omit()

#Frequency of types of crime by borough
borough.rate <- crime16 %>%
  group_by(borough) %>%
  summarize(Count=n())

borough.rate$pop <- employment$Population
borough.rate <- borough.rate %>%
  mutate(norm = Count/pop*100) %>%
  arrange(desc(norm))

#Overall, the borough with fewest crimes per 100 people in 2016 was Wandsworth with 9.66.
#Westminster had the highest crime rate with 28.7 crimes per 100 people. This is likely to 
#be seriously skewed due to the number of tourists who visit the area and are not accounted
#for in the normalisation

crime.norm <- crime16 %>%
  group_by(borough, crime_type) %>%
  summarize(Count=n())

ggplot(crime.norm) +
  geom_bar(stat="identity",aes(x = borough, y = Count,fill = crime_type)) +
  theme(panel.grid = element_blank(), axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(lineheight=.8, face="bold")) +
  ggtitle("Crime by Borough")

#There is significant variation between crime in the boroughs with Westminster having the
#most recorded crimes (particularly for other-theft and theft-from-the-person) and
#Kensingston & Chelsea the least. 

crime.col <- c("ASB" = "sienna", "BT" = "red3", "BU" = "orange", "CDA" = "yellow", "DR" = "lightgreen", "OC" = "limegreen", "OT" = "green", "PDW" = "olivedrab", "PO" = "royalblue", "PoW" = "lightblue", "RO" = "aquamarine", "SL" = "lightpink", "TP" = "violet", "VEC" = "deeppink", "VIC" = "grey")
ggplot(monthlycrime16, aes(x = month, y = Count)) +
  geom_point(aes(col = crime_abb)) +
  geom_line(aes(col = crime_abb)) +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black")) +
  scale_colour_manual(values = crime.col)

#The anti-social behaviour peak in summer is very clear still across the inner borough dataset.

#Heatmap by month - seasonal analysis
ggplot(monthlycrime16,aes(x=crime_abb,y=month,fill=Count))+
  geom_tile(aes(fill=Count))

#Violent crime and vehicle crime are also more prevalent in May-July.

#Kernel Density Estimation can be used to create 'hotspot' maps illustrating where the
#crime density is highest
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
  addPolygons(color = heat.colors(nlevs, NULL)[levs]) %>%
addLegend(color = heat.colors(nlevs, NULL)[levs])

#The heat map shows a concentration of crimes in Russell Square (SOAS, Birkbeck and UCL universities), Shoreditch and Dalston

#The process will be repeated for burglary
burglary16 <- crime16 %>%
  filter(crime_type == 'burglary')

#Add jitter in lat long
for(i in 1:length(burglary16$latitude)){
  burglary16$longitude[i] <- burglary16$longitude[i]+runif(1,0.001,0.004)
  burglary16$latitude[i] <- burglary16$latitude[i]+runif(1,0.001,0.004)}

#Heatmap using kernel density estimation
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

#The heat map shows a high concentration of burglaries in Fitzrovia and into Soho,
#with Brixton, Paddington and Shoreditch also showing more minor hotspots
