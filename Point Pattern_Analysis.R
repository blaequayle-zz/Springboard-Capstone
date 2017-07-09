library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(gridExtra)
library(spatstat)
library(tidyverse)

# Point Pattern Analysis based on: http://rspatial.org/analysis/rst/8-pointpat.html

setwd("/Users/Blae/Documents/Work/Data Science/Capstone_Project/")

#Transformations
#We need to have the same CRS for both crime points and the boroughs polygons
#wgs84 seems to work well for the boroughs shape files

wgs.84       <- "+proj=longlat +datum=WGS84"

setwd("/Users/Blae/Documents/Work/Data Science/gis_boundaries/ESRI/")
boroughs <- readOGR(dsn = "statistical-gis-boundaries-london/ESRI", "London_Borough_Excluding_MHW", verbose = FALSE)
central <- boroughs %>% 
  subset(NAME %in% c('Camden','Greenwich','Hackney', 'Hammersmith and Fulham','Islington', 'Kensington and Chelsea','Lambeth','Lewisham', 'Southwark','Tower Hamlets','Wandsworth','Westminster'))

#central <- spTransform(central, CRS(wgs.84))

#Now read in the crim e file
crime16 <- readRDS('crime16.rds')
coords <- SpatialPoints(crime16[,c("longitude","latitude")])
crime16 <- SpatialPointsDataFrame(coords, crime16)
proj4string(crime16) <- CRS(wgs.84)
crime16 <- spTransform(crime16, CRS(proj4string(central)))

plot(crime16)
plot(central, border = "blue", add = T)

crime_types <- sort(table(crime16$crime_type))[-1]

#Other-crime least prevalent, anti-social behaviour most prevalent

#Get coordinates and remove duplicated crime locations

xy <- coordinates(crime16)
dim(xy)
#447,418 sets of coordinates
xy <- unique(xy)
dim(xy)
#18,811 unique sets of coordinates
head(xy)

#Basic statistics - computing mean centre and standard distance for the crime data.
mc <- apply(xy, 2, mean)
sd <- sqrt(sum((xy[,1] - mc[1])^2 + (xy[,2] - mc[2])^2) / nrow(xy))

#Plot data and add summary circle by dividing the circle in 360 points and compute bearing in radians
plot(central, col='light blue')
points(crime16)
points(cbind(mc[1], mc[2]), pch='*', col='red', cex=0.5)
bearing <- 1:360 * pi/180
cx <- mc[1] + sd * cos(bearing)
cy <- mc[2] + sd * sin(bearing)
circle <- cbind(cx, cy)
lines(circle, col='red', lwd=2)

#Computing point density
CityArea <- sum(area(central)) #Area of inner London in metres squared
dens <- nrow(xy) / CityArea #6.314x10-5 crime locations per m2 or 63 crime locations per km2

#Compute quadrat counts - extent for the raster from the city polygon, and then assign arbitrary resolution of 1000
r <- raster(central)
res(r) <- 1000
r

#Find cells that are in the city - create polygons from the RasterLayer
r <- rasterize(central, r)
plot(r)
quads <- as(r, 'SpatialPolygons')
plot(quads, add=TRUE)
points(crime16, col='red',cex=0.1)


#Count no. events in each quadrat using ‘rasterize’ function. 
#Function can be used to summarize the number of points within each cell, but also to compute statistics based on the ‘marks’
#For example we could compute the number of different crime types by changing the ‘fun’ argument to another function

nc <- rasterize(coordinates(crime16), r, fun='count', background=0)
plot(nc)
plot(central, add=TRUE)

#Remove crimes outside of inner borough limits
ncrimes <- mask(nc, r)
plot(ncrimes)
plot(central, add=TRUE)










### Method 2 - spatstat - drugs in Inner London (https://www.r-bloggers.com/introductory-point-pattern-analysis-of-open-crime-data-in-london/)

Drugs <- crime16[crime16$crime_type==unique(crime16$crime_type)[5],]

#Duplicates have to be removed for spatstat to work - this is problematic as anonymised location points are used.
Drugs <- remove.duplicates(Drugs)

#A point pattern is defined as a series of events in a given area, or window, of observation
#It is therefore extremely important to precisely define this window. 
window <- as.owin.SpatialPolygons(central)
#Create the point pattern object:
Drugs.ppp <- ppp(x=Drugs@coords[,1], y=Drugs@coords[,2], window=window)

#How many events are in a predefined window? 
#Define this with Intensity - average number of events per unit area.

InnerLondonUTM <- spTransform(central,CRS("+init=epsg:32630"))

Drugs.ppp$n/sum(sapply(slot(InnerLondonUTM, "polygons"), slot, "area"))

#For drug related crime the average intensity is 2.87×10^-5 per square meter
#Most often the intensity is not spatially uniform, in that case the process is inhomogeneous
#Need to determine spatial variation of intensity.

Local.Intensity <- data.frame(Borough=factor(),Number=numeric())
for(i in unique(InnerLondonUTM$name)){
  sub.pol <- InnerLondonUTM[InnerLondonUTM$name==i,]
  
  sub.ppp <- ppp(x=Drugs.ppp$x,y=Drugs.ppp$y,window=as.owin.SpatialPolygons(sub.pol))
  Local.Intensity <- rbind(Local.Intensity,data.frame(Borough=factor(i,levels=InnerLondonUTM$name),Number=sub.ppp$n))
}

###NOT WORKING###
library(plotrix)
colorScale <- color.scale(Local.Intensity[order(Local.Intensity[,2]),2],color.spec="rgb",extremes=c("green","red"),alpha=0.8)
par(mar=c(5,13,4,2)) 
barplot(Local.Intensity[order(Local.Intensity[,2]),2],names.arg=Local.Intensity[order(Local.Intensity[,2]),1],horiz=T,las=2,space=1,col=colorScale)


#Spatial distribution of the intensity is by using kernel smoothing 
#Computes the intensity continuously across the study area. 
#Need to define the bandwidth of the density estimation, which basically determines the area of influence of the estimation

plot(density.ppp(Drugs.ppp, sigma = bw.diggle(Drugs.ppp),edge=T),main=paste("h =",round(bw.diggle(Drugs.ppp),2)))
plot(density.ppp(Drugs.ppp, sigma = bw.ppl(Drugs.ppp),edge=T),main=paste("h =",round(bw.ppl(Drugs.ppp),2)))
plot(density.ppp(Drugs.ppp, sigma = bw.scott(Drugs.ppp)[2],edge=T),main=paste("h =",round(bw.scott(Drugs.ppp)[2],2)))
plot(density.ppp(Drugs.ppp, sigma = bw.scott(Drugs.ppp)[1],edge=T),main=paste("h =",round(bw.scott(Drugs.ppp)[1],2)))
