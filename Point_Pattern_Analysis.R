library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(gridExtra)
library(spatstat)
library(tidyverse)
library(readr)

# Point Pattern Analysis based on: http://rspatial.org/analysis/rst/8-pointpat.html

setwd("/Users/Blae/Documents/Work/Data Science/Capstone_Project/")

#Transformations
#We need to have the same CRS for both crime points and the boroughs polygons
#wgs84 seems to work well for the boroughs shape files

wgs.84       <- "+proj=longlat +datum=WGS84"

setwd("/Users/Blae/Documents/Work/Data Science/gis_boundaries/ESRI/")
boroughs <- readOGR(dsn = ".", "London_Borough_Excluding_MHW", verbose = FALSE)
central <- boroughs %>% 
  subset(NAME %in% c('Camden','Greenwich','Hackney', 'Hammersmith and Fulham','Islington', 'Kensington and Chelsea','Lambeth','Lewisham', 'Southwark','Tower Hamlets','Wandsworth','Westminster'))

#Now read in the crime file
setwd("/Users/Blae/Documents/Work/Data Science/Capstone_Project/")
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

#Calculating frequencies
f <- freq(nc, useNA='no')
head(f)
plot(f, pch=20)

#Does this look like a pattern you would have expected?
# Average number of cases per quadrat
quadrats <- sum(f[,2])
cases <- sum(f[,1] * f[,2])
mu <- cases / quadrats
mu
## [1] 872.1598

ff <- data.frame(f)
colnames(ff) <- c('K', 'X')
ff$Kmu <- ff$K - mu
ff$Kmu2 <- ff$Kmu^2
ff$XKmu2 <- ff$Kmu2 * ff$X
head(ff)

#Oberved variance s2 and VMR is:
s2 <- sum(ff$XKmu2) / (sum(ff$X)-1)
s2
## [1] 1542407
VMR <- s2 / mu
VMR
## [1] 1768.492

#The point pattern is exceptionally concentrated within one quadrat.

#Distance based measures - requires planar coordinate based system.
d <- dist(xy)
class(d)

#Coerce the dist object to a matrix, and ignore distances from each point to itself (0,0)
dm <- as.matrix(d)
dm[1:5, 1:5]
diag(dm) <- NA
dm[1:5, 1:5]

#Get minimum distance to another event, we can use the ‘apply’ function. Think of the rows as each point, and the columns of all other points (vice versa could also work).

dmin <- apply(dm, 1, min, na.rm=TRUE)
head(dmin)

#Mean nearest neighbour distance 
mdmin <- mean(dmin)
#Which point is its nearest neighbour? Use the ‘which.min’ function.  
wdmin <- apply(dm, 1, which.min)
#What are the most isolated cases? That is the furtest away from their nearest neigbor.
plot(city)
points(crime, cex=.1)
ord <- rev(order(dmin))
far25 <- ord[1:25]
neighbors <- wdmin[far25]
points(xy[far25, ], col='blue', pch=20)
points(xy[neighbors, ], col='red')

#Drawing the lines, easiest via a loop
for (i in far25) {
  lines(rbind(xy[i, ], xy[wdmin[i], ]), col='red')
}

#G function

max(dmin)
## [1] 1829.738
# get the unique distances (for the x-axis)
distance <- sort(unique(round(dmin)))
# compute how many cases there with distances smaller that each x
Gd <- sapply(distance, function(x) sum(dmin < x))
# normalize to get values between 0 and 1
Gd <- Gd / length(dmin)
plot(distance, Gd)

# using xlim to exclude the extremes
plot(distance, Gd, xlim=c(0,500))

#Here is a function to show these values in a more standard way.

stepplot <- function(x, y, type='l', add=FALSE, ...) {
  x <- as.vector(t(cbind(x, c(x[-1], x[length(x)]))))
  y <- as.vector(t(cbind(y, y)))
  if (add) {
    lines(x,y, ...)
  } else {
    plot(x,y, type=type, ...)
  }
}
#And use it for our G function data.

stepplot(distance, Gd, type='l', lwd=2, xlim=c(0,500))

#The steps are so small in our data, that you hardly see the difference.

#I use the centers of previously defined raster cells to compute the F function.

# get the centers of the 'quadrats' (raster cells)
p <- rasterToPoints(r)
# compute distance from all crime sites to these cell centers
d2 <- pointDistance(p[,1:2], xy, longlat=FALSE)

# the remainder is similar to the G function
Fdistance <- sort(unique(round(d2)))
mind <- apply(d2, 1, min)
Fd <- sapply(Fdistance, function(x) sum(mind < x))
Fd <- Fd / length(mind)
plot(Fdistance, Fd, type='l', lwd=2, xlim=c(0,3000))

#Compute the expected distributon

ef <- function(d, lambda) {
  E <- 1 - exp(-1 * lambda * pi * d^2)
}
expected <- ef(0:2000, dens)

#Combine F and G on one plot.
plot(distance, Gd, type='l', lwd=2, col='red', las=1,
     ylab='F(d) or G(d)', xlab='Distance', yaxs="i", xaxs="i")
lines(Fdistance, Fd, lwd=2, col='blue')
lines(0:2000, expected, lwd=2)

legend(1200, .3,
       c(expression(italic("G")["d"]), expression(italic("F")["d"]), 'expected'),
       lty=1, col=c('red', 'blue', 'black'), lwd=2, bty="n")

#Finally, let’s compute K. Note that I use the original distance matrix ‘d’ here.

distance <- seq(1, 30000, 100)
Kd <- sapply(distance, function(x) sum(d < x)) # takes a while
Kd <- Kd / (length(Kd) * dens)
plot(distance, Kd, type='l', lwd=2)


#Using spatstat package - first make a Kernel Density raster.

#Coerce from SpatialPolygons to an object of class “owin” (observation window)

cityOwin <- as.owin(central)
class(cityOwin)
## [1] "owin"
cityOwin ## window is a polygonal boundary
##Eclosing rectangle: [521054.9, 547612] x [169648, 188327.4] units

#Extract coordinates from SpatialPointsDataFrame:
  
pts <- coordinates(xy) #Have to use xy as duplicated points are not accepted
head(pts)
p <- ppp(pts[,1], pts[,2], window=cityOwin)
class(p)
## [1] "ppp"
p
##Planar point pattern: 18811 points
##window: polygonal boundary
##Enclosing rectangle: [521054.9, 547612] x [169648, 188327.4] units
plot(p)

#Compute Kernel Density
ds <- density(p)
class(ds)
plot(ds, main='crime density')

#Density is the number of points per unit area. Check if numbers make sense
nrow(pts)
## [1] 18811
r <- raster(ds)
s <- sum(values(r), na.rm=TRUE)
s * prod(res(r))
## [1] 19227 - Looks about right

#Create a marked point pattern object (ppp) for all crimes. It is important to coerce the marks to a factor variable.

crime16$fcat <- as.factor(crime16$crime_type)
w <- as.owin(central)
xy <- coordinates(crime16)
mpp <- ppp(xy[,1], xy[,2], window = w, marks=crime16$fcat)

#We can split the mpp object by category
spp <- split(mpp)
plot(spp[1:4], main='')

#Crime density by category:
  
plot(density(spp[1:4]), main='')

#Produce K-plots (with an envelope) for ‘bicycle-theft’ and ‘violent-crime’.
spatstat.options(checksegments = FALSE)
kbike <- Kest(spp$"bicycle-theft")
kebike <- envelope(spp$"bicycle-theft", Kest)

ktheft <- Kest(spp$"violent-crime")
ketheft <- envelope(spp$"violent-crime", Kest)

par(mfrow=c(1,2))
plot(ktheft)
plot(ketheft)

#Is population density a good predictor of bicycle theft and for violent crime? 
#One approach is to do a Kolmogorov-Smirnov (‘kstest’), using population density as a covariate:
  
KS.arson <- cdf.test(spp$Arson, ds)
KS.arson


#Spatial Kolmogorov-Smirnov test of CSR in two dimensions
##
## data:  covariate 'ds' evaluated at points of 'spp$Arson'
##      and transformed to uniform distribution under CSR
## D = 0.51156, p-value = 0.01038
## alternative hypothesis: two-sided
KS.drunk <- kstest(spp$'Drunk in Public', ds)
## kstest is out of date; use cdf.test
KS.drunk
##
##  Spatial Kolmogorov-Smirnov test of CSR in two dimensions
##
## data:  covariate 'ds' evaluated at points of 'spp$"Drunk in Public"'
##      and transformed to uniform distribution under CSR
## D = 0.54198, p-value < 2.2e-16
## alternative hypothesis: two-sided

#Compare patterns for “bicycle-theft” and for “violent-crime” with the KCross function.
kc <- Kcross(mpp, i = "bicycle-theft", j = "violent-crime")
ekc <- envelope(mpp, Kcross, nsim = 50, i = "bicycle-theft", j = "violent-crime")
plot(ekc)


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

