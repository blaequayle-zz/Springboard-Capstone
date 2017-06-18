#Load required library packages
library(readr)
library(leaflet)
library(ggplot2)
library(lubridate)
library(scales)
library(rgdal)
library(broom)
library(classInt)
library(KernSmooth)

#Read dataset
setwd("/Users/Blae/Documents/Work/Data Science/Capstone_Project/")
crime11to17 <- readRDS("crime11to17.rds")
monthly.crime <- readRDS("monthlycrime.rds")
annual.crime <- readRDS("annualcrime.rds")
seasonal.crime <- readRDS("seasonalcrime.rds")

#Initial exploration of data

summary(crime11to17)
table(crime11to17$crime_type)

#The most prevalent street crime in London, out of 15 categories, is anti-social behaviour

asb.count <- count(crime11to17, crime_type == "anti-social-behaviour")
total.count <- nrow(crime11to17)
print(asb.count[2,2]/total.count)

#It makes up 23.1% of recorded crime

#The majority of the records are from the Police Force but a small portion is submitted by the British Transport Police who operate on the railways
service.count <- count(crime11to17, service == "BTP")
print(service.count[2,2]/total.count)
#These records account for only 4.3%

#Investigation of the annual variation in street crimes by plotting a scatter graph with geom_smooth  
total.year <- crime11to17 %>% 
  group_by(year) %>%
  summarize(Count=n())

ann.crime.plot <- ggplot(annual.crime, aes(x = year, y = Count)) +
  geom_point(aes(col = crime_type)) +
  geom_line(aes(col = crime_type)) +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black"))

print(ann.crime.plot)

ann.crime.plot <- ann.crime.plot +   
  geom_point(total.year, aes(x = year, y = Count, col = "black")) #How do I add additional dataset?
  
#Three categories have shown a strong decrease between 2011 and 2017 - other-crime, other-theft and anti-social-behaviour 
#Background of continuously decreasing reported street crimes 
#The anonymously large drop between 2012 and 2013 may suggest a change in recording/reporting

ggplot(annual.crime) +
  geom_bar(stat="identity",aes(x = year, y = Count,fill = crime_type)) +
  theme(panel.grid = element_blank())

#Use Leaflet to visualise location of specific crime.
#Reduced dataset required due to processing capability, limited to around three months of data.

VC.2016 <- crime11to17 %>%
filter(year == 2016, crime_type == 'vehicle-crime')

VT.2016 %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude)


#Investigation of the monthly variation in street crimes
ggplot(monthly.crime) +
  geom_bar(stat="identity",aes(x = month,y = Count,fill = crime_type)) +
  facet_grid(year~.)

ggplot(monthly.crime) +
  geom_point(aes(x = month, y = Count, col = crime_type)) +
  facet_grid(year~.) +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Month") +
  ylab("Count") 

#Neither of these visualisations make for simple interpretation
#It appears anti social behaviour is usually more prevalent in the summer months
#Line graph with log10 scale for y axis
ggplot(monthly.crime, aes(x = month, y = Count), log = "y") +
  geom_line(aes(group = crime_type, col = crime_type)) +
  scale_y_log10() +
  facet_grid(year~.) 


##########DHIRAJ CODE#########
#Bicycle Thefts
bike <- crime11to17 %>%
  filter(crime_type == 'bicycle-theft')

#Add jitter in lat long
for(i in 1:length(bike$latitude)){
  bike$longitude[i] <- bike$longitude[i]+runif(1,0.001,0.004)
  bike$latitude[i] <- bike$latitude[i]+runif(1,0.001,0.004)}

leaflet(bike) %>%
  addTiles() %>% 
  addCircles(~longitude,~latitude)

#Heatmap by month
ggplot(monthly.crime,aes(x=crime_type,y=month,fill=Count))+
  geom_tile(aes(fill=Count))

#Heatmap
latlong <- select(bike, longitude, latitude)
kde <- bkde2D(latlong,
              bandwidth=c(.0055, .0048), gridsize = c(75,75))
CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
## EXTRACT CONTOUR LINE LEVELS
LEVS <- as.factor(sapply(CL, `[[`, "level"))
NLEV <- length(levels(LEVS))

## CONVERT CONTOUR LINES TO POLYGONS
library(sp)
pgons <- lapply(1:length(CL), function(i)
  Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons = SpatialPolygons(pgons)
leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(NLEV, NULL)[LEVS]) #%>%
# addCircles(lng = latlong$longitude, lat = latlong$latitude,
#            radius = .5, opacity = .2, col = "blue")

#The heat map shows a concetration of crimes in Covent Garden and on South Bank.