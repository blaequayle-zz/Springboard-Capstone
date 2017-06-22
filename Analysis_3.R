#Analysis 3 is focussing on chloropleth maps using the inner borough dataset
#Load required packages
library(rgdal)
library(rgeos)
library(dplyr)
library(sp)
library(classInt)
library(ggplot2)
library(maptools)
library(RColorBrewer)
library(ggmap)
library(leaflet)

#Set working directory, load crime data, read in shape file data, subset to inner boroughs
setwd("/Users/Blae/Documents/Work/Data Science/gis_boundaries/ESRI/")
boroughs <- readOGR(dsn = ".", "London_Borough_Excluding_MHW", verbose = FALSE)
central <- boroughs %>% 
  subset(NAME %in% c('Camden','Greenwich','Hackney', 'Hammersmith and Fulham','Islington', 'Kensington and Chelsea','Lambeth','Lewisham', 'Southwark','Tower Hamlets','Wandsworth','Westminster'))

#Load inner borough crime data set - original
setwd("/Users/Blae/Documents/Work/Data Science/Capstone_Project/")
crime16 <- readRDS("crime16.rds")

#Convert crime data to a SpatialPointsDataFrame with latlong projection
coords <- SpatialPoints(crime16[,c("longitude","latitude")])
crime16 <- SpatialPointsDataFrame(coords, crime16)
proj4string(crime16) <- CRS("+init=epsg:4326")

#Transform borough boundaries to geographic coordinates (latitude/longitude)
central <- spTransform(central, CRS("+init=epsg:4326"))
central@proj4string

#Investigating Bicycle Theft
#Subset the bicycle theft offences and count the number of points within boroughs using over()
#over(x, y) at the spatial locations of object x retrieve the indexes or attributes from spatial object y
bicycle_theft <- crime16[crime16$crime_type == "bicycle-theft",]
proj4string(crime16) <- proj4string(central)
proj4string(bicycle_theft) <- proj4string(central)

pointsinpolygon <- over(SpatialPolygons(central@polygons), SpatialPoints(bicycle_theft), returnList = TRUE)
central$bicycle_theft <- unlist(lapply(pointsinpolygon, length))

#Calculate thematic ranges using natural breaks using classIntervals(), used with classes$brks
classes <- classIntervals(central$bicycle_theft, n=5, style="jenks")
#Removed != 0 need to add back in if broaden data set

#Fortify the borough shapefile for plotting in ggplot2
central_f <- fortify(central, region="NAME")
central_f <- left_join(central_f, central@data, by = c("id" = "NAME"))

#Plot a chloropleth map of borough level bicycle theft using ggplot2
ggplot() +
  geom_polygon(data = central_f, aes(x = long, y = lat, group = group,
                                      fill = bicycle_theft), color = "white", size = 0.2) +
  coord_map() +
  scale_fill_gradientn('Frequency\n', colours = brewer.pal(5,"YlOrRd"), breaks = classes$brks) +
  theme_nothing(legend = TRUE) +
  labs(title = "Reported bicycle thefts by London borough (2016)")

#This looks good but it would be better if it was interactive
#For an interactive map, first need to set the text that pops up using html:
central_popup <- paste0("<strong>Borough: </strong>",
                         central$NAME,
                         "<br><strong>Offences (2016): </strong>",
                         central$bicycle_theft)
#Make colour tables
colcode2 <- findColours(classes, c("darkgrey", "yellow", "orange", "red", "sienna"))

#Make labels for legend
breaks <- round(classes$brks, 1)
labels = matrix(1:(length(breaks)-1))
for(j in 1:length(labels)){labels [j] =
  paste(as.character(breaks[j]),"-",as.character(breaks[j+1]))}

#Create plot using OpenStreetMap raster layer using leaflet package
leaflet(data = central) %>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons(data = central, 
              fillColor = colcode2, 
              fillOpacity = 0.6, 
              color = "#636363", 
              weight = 2, 
              popup = central_popup)  %>%
  addLegend(position = "bottomright",
            colors = c("darkgrey", "yellow", "orange", "red", "sienna"),
            labels = labels,
            opacity = 0.8,
            title = "Number of bike thefts")

#Repeat this process and investigate violent crime
violent_crime <- crime16[crime16$crime_type == "violent-crime",]
proj4string(crime16) <- proj4string(central)
proj4string(violent_crime) <- proj4string(central)

pointsinpolygon <- over(SpatialPolygons(central@polygons), SpatialPoints(violent_crime), returnList = TRUE)
central$violent_crime <- unlist(lapply(pointsinpolygon, length))

#Calculate thematic ranges using natural breaks using classIntervals(), used with classes$brks
classes <- classIntervals(central$violent_crime[central$violent_crime!= 0], n=5, style="jenks")
#Removed != 0 need to add back in if broaden data set

#Fortify the borough shapefile again for plotting in ggplot2
central_f <- fortify(central, region="NAME")
central_f <- left_join(central_f, central@data, by = c("id" = "NAME"))

#Plot a chloropleth map of violent crime using ggplot2
ggplot() +
  geom_polygon(data = central_f, aes(x = long, y = lat, group = group,
                                     fill = violent_crime), color = "white", size = 0.2) +
  coord_map() +
  scale_fill_gradientn('Frequency\n', colours = brewer.pal(5,"YlOrRd"), breaks = classes$brks) +
  theme_nothing(legend = TRUE) +
  labs(title = "Reported violent crimes by London borough (2016)")

#Interactive chloropleth map of violent crime
central_popup <- paste0("<strong>Borough: </strong>",
                        central$NAME,
                        "<br><strong>Offences (2016): </strong>",
                        central$violent_crime)

#Setting colour fill for polygons
colcode2 <- findColours(classes, c("darkgrey", "yellow", "orange", "red", "sienna"))

#Make labels for legend
breaks <- round(classes$brks, 1)
labels = matrix(1:(length(breaks)-1))
for(j in 1:length(labels)){labels [j] =
  paste(as.character(breaks[j]),"-",as.character(breaks[j+1]))}

#Create plot using OpenStreetMap raster layer using leaflet package
leaflet(data = central) %>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons(data = central, 
              fillColor = colcode2, 
              fillOpacity = 0.6, 
              color = "#636363", 
              weight = 2, 
              popup = central_popup)  %>%
  addLegend(position = "bottomright",
            colors = c("darkgrey", "yellow", "orange", "red", "sienna"),
            labels = labels,
            opacity = 0.8,
            title = "Number of violent crimes")


#Repeat this process to investigate burglaries
burglary <- crime16[crime16$crime_type == "burglary",]
proj4string(crime16) <- proj4string(central)
proj4string(burglary) <- proj4string(central)

pointsinpolygon <- over(SpatialPolygons(central@polygons), SpatialPoints(burglary), returnList = TRUE)
central$burglary <- unlist(lapply(pointsinpolygon, length))

#Calculate thematic ranges using natural breaks using classIntervals(), used with classes$brks
classes <- classIntervals(central$burglary[central$burglary != 0], n=5, style="jenks")
#Removed != 0 need to add back in if broaden data set

#Fortify the borough shapefile again for plotting in ggplot2
central_f <- fortify(central, region="NAME")
central_f <- left_join(central_f, central@data, by = c("id" = "NAME"))

#Plot a chloropleth map of burglaries using ggplot2
ggplot() +
  geom_polygon(data = central_f, aes(x = long, y = lat, group = group,
                                     fill = burglary), color = "white", size = 0.2) +
  coord_map() +
  scale_fill_gradientn('Frequency\n', colours = brewer.pal(5,"YlOrRd"), breaks = classes$brks) +
  theme_nothing(legend = TRUE) +
  labs(title = "Reported burglaries by London borough (2016)")

#Interactive chloropleth map of burglaries
central_popup <- paste0("<strong>Borough: </strong>",
                        central$NAME,
                        "<br><strong>Offences (2016): </strong>",
                        central$burglary)

#Setting colour fill for polygons
colcode2 <- findColours(classes, c("darkgrey", "yellow", "orange", "red", "sienna"))

#Make labels for legend
breaks <- round(classes$brks, 1)
labels = matrix(1:(length(breaks)-1))
for(j in 1:length(labels)){labels [j] =
  paste(as.character(breaks[j]),"-",as.character(breaks[j+1]))}

#Create plot using OpenStreetMap raster layer using leaflet package
leaflet(data = central) %>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons(data = central, 
              fillColor = colcode2, 
              fillOpacity = 0.6, 
              color = "#636363", 
              weight = 2, 
              popup = central_popup)  %>%
  addLegend(position = "bottomright",
            colors = c("darkgrey", "yellow", "orange", "red", "sienna"),
            labels = labels,
            opacity = 0.8,
            title = "Number of burglaries")

#In terms of tourists, the crime which may potentially impact them directly is theft from the person.

theft.from.the.person <- crime16[crime16$crime_type == "theft-from-the-person",]
proj4string(crime16) <- proj4string(central)
proj4string(theft.from.the.person) <- proj4string(central)

pointsinpolygon <- over(SpatialPolygons(central@polygons), SpatialPoints(theft.from.the.person), returnList = TRUE)
central$theft.from.the.person <- unlist(lapply(pointsinpolygon, length))

#Calculate thematic ranges using natural breaks using classIntervals(), used with classes$brks
classes <- classIntervals(central$theft.from.the.person[central$theft.from.the.person != 0], n=5, style="jenks")
#Removed != 0 need to add back in if broaden data set

#Fortify the borough shapefile again for plotting in ggplot2
central_f <- fortify(central, region="NAME")
central_f <- left_join(central_f, central@data, by = c("id" = "NAME"))

#Plot a chloropleth map of theft from the person using ggplot2
ggplot() +
  geom_polygon(data = central_f, aes(x = long, y = lat, group = group,
                                     fill = theft.from.the.person), color = "white", size = 0.2) +
  coord_map() +
  scale_fill_gradientn('Frequency\n', colours = brewer.pal(5,"YlOrRd"), breaks = classes$brks) +
  theme_nothing(legend = TRUE) +
  labs(title = "Reported thefts from the person by London borough (2016)")

#Interactive chloropleth map of theft from the person
central_popup <- paste0("<strong>Borough: </strong>",
                        central$NAME,
                        "<br><strong>Offences (2016): </strong>",
                        central$theft.from.the.person)

#Setting colour fill for polygons
colcode2 <- findColours(classes, c("darkgrey", "yellow", "orange", "red", "sienna"))

#Make labels for legend
breaks <- round(classes$brks, 1)
labels = matrix(1:(length(breaks)-1))
for(j in 1:length(labels)){labels [j] =
  paste(as.character(breaks[j]),"-",as.character(breaks[j+1]))}

#Create plot using OpenStreetMap raster layer using leaflet package
leaflet(data = central) %>% 
  addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  addPolygons(data = central, 
              fillColor = colcode2, 
              fillOpacity = 0.6, 
              color = "#636363", 
              weight = 2, 
              popup = central_popup)  %>%
  addLegend(position = "bottomright",
            colors = c("darkgrey", "yellow", "orange", "red", "sienna"),
            labels = labels,
            opacity = 0.8,
            title = "Number of thefts from the person")

#The highest occurences of theft from a person are in Westminster, 6101 records in 2016
#This borough contains many of the capitals major tourist attractions including Buckingham Palace,
#London Zoo and Covent Garden.

#Ideally, these plots would be adjusted to account for factors such as population and area of each borough
#You would expect larger populations to lead to more recorded crimes 











