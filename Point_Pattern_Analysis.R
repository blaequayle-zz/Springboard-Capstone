library(raster)
library(rgdal)
library(rgeos)
library(maptools)
library(gridExtra)
library(spatstat)
library(tidyverse)
library(readr)

# Point Pattern Analysis based on: http://rspatial.org/analysis/rst/8-pointpat.html

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

#Filter out only burglaries as dataset too large for this method otherwise
burglary16 <- crime16 %>%
  filter(crime_type == "burglary")

coords <- SpatialPoints(burglary16[,c("longitude","latitude")])
burglary16 <- SpatialPointsDataFrame(coords, burglary16)
proj4string(burglary16) <- CRS(wgs.84)
burglary16 <- spTransform(burglary16, CRS(proj4string(central)))

plot(burglary16)
plot(central, border = "blue", add = T)

crime_types <- sort(table(crime16$crime_type))[-1]

#Other-crime least prevalent, anti-social behaviour most prevalent

#Get coordinates and remove duplicated crime locations

xy <- coordinates(burglary16)
dim(xy)
#28515 sets of coordinates
xy <- unique(xy)
dim(xy)
#11733 unique sets of coordinates - due to set location points for crimes
head(xy)

#Basic statistics - computing mean centre and standard distance for the crime data.
mc <- apply(xy, 2, mean)
sd <- sqrt(sum((xy[,1] - mc[1])^2 + (xy[,2] - mc[2])^2) / nrow(xy))

#Plot data and add summary circle by dividing the circle in 360 points and compute bearing in radians
plot(central, col='light blue')
points(burglary16) #The city of London gap is very clear
points(cbind(mc[1], mc[2]), pch='*', col='red', cex=0.5)
bearing <- 1:360 * pi/180
cx <- mc[1] + sd * cos(bearing)
cy <- mc[2] + sd * sin(bearing)
circle <- cbind(cx, cy)
lines(circle, col='red', lwd=2)

#Computing point density
CityArea <- sum(area(central)) #Area of inner London in metres squared
dens <- nrow(xy) / CityArea #3.9x10-5 crime locations per m2 or 39 per km2

#Compute quadrat counts - extent for the raster from the city polygon, and then assign arbitrary resolution of 1000
r <- raster(central)
res(r) <- 1000
r

#Find cells that are in the city - create polygons from the RasterLayer
r <- rasterize(central, r)
plot(r)
quads <- as(r, 'SpatialPolygons')
plot(quads, add=TRUE)
points(burglary16, col='red',cex=0.1)

#Count no. events in each quadrat using ‘rasterize’ function. 
#Function can be used to summarize the number of points within each cell, but also to compute statistics based on the ‘marks’

nc <- rasterize(coordinates(burglary16), r, fun='count', background=0)
plot(nc)
plot(central, add=TRUE)

#Calculating frequency of crime locations in each quadrat
f <- freq(nc, useNA='no')
head(f)
plot(f, pch=20)

#Does this look like a pattern you would have expected?
#The 165 quadrats with no crime locations are all the null ones that lie outside the boroughs
#There are 7 quadrats with over 240 different crime locations
#There is a high concentration of quadrats with up to 100 crime locations.

#Average number of crime locations per quadrat
quadrats <- sum(f[,2])
cases <- sum(f[,1] * f[,2])
mu <- cases / quadrats
mu
## [1] 55.5848

ff <- data.frame(f)
colnames(ff) <- c('K', 'X')
ff$Kmu <- ff$K - mu
ff$Kmu2 <- ff$Kmu^2
ff$XKmu2 <- ff$Kmu2 * ff$X
head(ff)

#Oberved variance s2 and Variance to Mean Ratio (VMR) is:
s2 <- sum(ff$XKmu2) / (sum(ff$X)-1)
s2
## [1] 4384.5
VMR <- s2 / mu
VMR
## [1] 78.9
#This is a unit-less statistic describing the spatial arrangement of points. 
#In general stratified distributions ~ 0, random distributions = 1 and clustered distributions a VMR of above 1.
#This distribution is highly clustered.

#Distance based measures - requires planar coordinate based system.
d <- dist(xy)
class(d)

#Coerce the dist object to a matrix, and ignore distances from each point to itself (0,0)
dm <- as.matrix(d)
dm[1:5, 1:5]
diag(dm) <- NA
dm[1:5, 1:5]

#Get minimum distance to another event, we can use the ‘apply’ function. Rows as each point, and the columns of all other points (vice versa could also work).

dmin <- apply(dm, 1, min, na.rm=TRUE)
head(dmin)

#Mean nearest neighbour distance is 83m
mdmin <- mean(dmin)

#Which point is its nearest neighbour?
wdmin <- apply(dm, 1, which.min)

#What are the most isolated cases - top 25 most isolated cases from their nearest neighbour 
plot(central)
points(burglary16, cex=.1)
ord <- rev(order(dmin))
far25 <- ord[1:25]
neighbours <- wdmin[far25]
points(xy[far25, ], col='blue', pch=20)
points(xy[neighbours, ], col='red')

#Drawing the lines, easiest via a loop
for (i in far25) {
  lines(rbind(xy[i, ], xy[wdmin[i], ]), col='red')
}

#G function - cumulative frequency distribution of the nearest neighbor distance
#Probability for a specified distance, that the nearest neighbor distance to another event in the pattern will be less than the specified distance.

max(dmin) ## [1] 854.2938

#Get the unique distances (for the x-axis)
distance <- sort(unique(round(dmin)))

#Compute how many cases there with distances smaller that each x
Gd <- sapply(distance, function(x) sum(dmin < x))
#Normalise to get values between 0 and 1
Gd <- Gd / length(dmin)
plot(distance, Gd)

#Using xlim to exclude the extremes
plot(distance, Gd, xlim=c(0,300))

#Evenly-spaced events, G(d) rises gradually up to the distance at which most events are spaced, and then increases rapidly
#Clustered events, G(d) rises rapidly at short distances, and then levels off at larger d-values

#The distribution is more clustered than evenly spaced.

#A function to show these values in a more standard way.

stepplot <- function(x, y, type='l', add=FALSE, ...) {
  x <- as.vector(t(cbind(x, c(x[-1], x[length(x)]))))
  y <- as.vector(t(cbind(y, y)))
  if (add) {
    lines(x,y, ...)
  } else {
    plot(x,y, type=type, ...)
  }
}
#And for the G function data.

stepplot(distance, Gd, type='l', lwd=2, xlim=c(0,300))

#Both plots appear very similar.
#Use the centres of previously defined raster cells to compute the F function.
p <- rasterToPoints(r)
#Compute distance from all crime sites to these cell centers
d2 <- pointDistance(p[,1:2], xy, longlat=FALSE)

#The remainder is similar to the G function
Fdistance <- sort(unique(round(d2)))
mind <- apply(d2, 1, min)
Fd <- sapply(Fdistance, function(x) sum(mind < x))
Fd <- Fd / length(mind)
plot(Fdistance, Fd, type='l', lwd=2, xlim=c(0,1000))

#Evenly-spaced events, F(d) rises rapidly up to the distance at which most events are spaced, and then levels off (more nearest neighbors at small distances from randomly placed points)
#Clustered events, F(d) rises rapidly at short distances, and then levels off at larger d-values

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

#Compute K using original distance matrix ‘d’

distance <- seq(1, 30000, 100)
Kd <- sapply(distance, function(x) sum(d < x))
Kd <- Kd / (length(Kd) * dens)
plot(distance, Kd, type='l', lwd=2)


#Using spatstat package - first make a Kernel Density raster.

#Coerce from SpatialPolygons to an object of class “owin” (observation window)
cityOwin <- as.owin(central)
class(cityOwin)

##enclosing rectangle: [521054.9, 547612] x [169648, 188327.4] units

#Extract coordinates from SpatialPointsDataFrame:
  
pts <- coordinates(xy) #Have to use xy as duplicated points are not accepted
head(pts)
p <- ppp(pts[,1], pts[,2], window=cityOwin)
class(p)
p
##Planar point pattern: 11733 points
##window: polygonal boundary
##enclosing rectangle: [521054.9, 547612] x [169648, 188327.4] units
plot(p)

#Compute Kernel Density
ds <- density(p)
class(ds)
plot(ds, main='Burglary density')

#Density is the number of points per unit area. Check if numbers make sense
nrow(pts)
r <- raster(ds)
s <- sum(values(r), na.rm=TRUE)
s * prod(res(r))
## [1] 12009 - Looks about right

#Create a marked point pattern object (ppp). It is important to coerce the marks to a factor variable.
burglary16$fcat <- as.factor(burglary16$crime_type)
w <- as.owin(central)
xy <- coordinates(burglary16)
mpp <- ppp(xy[,1], xy[,2], window = w, marks=burglary16$fcat)
spp <- split(mpp)


#Produce K-plots (with an envelope) for burglary
spatstat.options(checksegments = FALSE)
kburglary <- Kest(spp$"burglary")
keburglary <- envelope(spp$"burglary", Kest)

par(mfrow=c(1,2))
plot(kburglary)
plot(keburglary)

#Is population density a good predictor of burglary? 
#One approach is to do a Kolmogorov-Smirnov (‘kstest’), using population density as a covariate:
  
KS.burglary <- cdf.test(spp$burglary, ds)
KS.burglary

#Spatial Kolmogorov-Smirnov test of CSR in two dimensions
#Data: covariate ‘ds’ evaluated at points of ‘spp$burglary’ and transformed to uniform distribution under CSR
#D = 0.20153, p-value < 2.2e-16
#alternative hypothesis: two-sided


