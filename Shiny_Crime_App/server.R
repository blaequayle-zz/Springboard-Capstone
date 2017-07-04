shinyServer(function(input, output) {
  library(dplyr)
  library(ggmap)
  library(lubridate)
  library(leaflet)
  library(sp)
  library(rgdal)
  library(maptools)
  library(KernSmooth)
  
  latlon <- reactive({
    filter(bike16, bike16$date>=input$dateRange[1]&bike16$date<=input$dateRange[2])
  })
  
  output$popmap <- renderLeaflet({
    leaflet(bike16) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observe({
    leafletProxy("popmap", data = latlon()) %>%
      clearShapes() %>%
      clearControls() %>%
      addCircles(~longitude,~latitude,color="blue")
  })
  


  
  output$heatmap <- renderLeaflet({
    latlong <- select(bike16, longitude,latitude)
    latlong <- filter(latlong, bike16$date>=input$dateRange1[1]&bike16$date<=input$dateRange1[2])
    kde <- bkde2D(latlong,
                  bandwidth=c(.0045, .0068), gridsize = c(75,75))
    CL <- contourLines(kde$x1 , kde$x2 , kde$fhat)
    
    ## EXTRACT CONTOUR LINE LEVELS
    LEVS <- as.factor(sapply(CL, `[[`, "level"))
    NLEV <- length(levels(LEVS))
    
    ## CONVERT CONTOUR LINES TO POLYGONS
    pgons <- lapply(1:length(CL), function(i)
      Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
    spgons = SpatialPolygons(pgons)
    leaflet(spgons) %>% addTiles() %>%
      addPolygons(color = heat.colors(NLEV, NULL)[LEVS]) %>%
      addCircles(lng = latlong$longitude, lat = latlong$latitude,
                 radius = .5, opacity = .2, col = "blue")
  })
  
  output$safetymap <- renderLeaflet({
    leaflet() %>%
      setView(lng = -0.1288, lat = 51.5102, zoom=11) %>%
      addTiles(options = providerTileOptions(noWrap = TRUE))
  })
  
  ## Observe mouse clicks and add circles
  observeEvent(input$safetymap_click, {
    ## Get the click info like had been doing
    click <- input$safetymap_click
    clat <- click$lat
    clng <- click$lng
    #address <- revgeocode(c(clng,clat))
    thefts <- nrow(filter(bike16, latitude>=(clat-0.0089)&latitude<=(clat+0.0089)&
                    longitude>=(clng-0.0089/cos(clat*0.018))&longitude<=(clng+0.0089/cos(clat*0.018))))
    leafletProxy("safetymap") %>% # use the proxy to save computation
      addCircles(lng=clng, lat=clat, group='circles',
                 weight=1, radius=1000, color='black', fillColor=ifelse(thefts<200,'green',ifelse(thefts>=200&thefts<400,'orange', ifelse(thefts>=400, 'red'))),
                 fillOpacity=0.5, opacity=1,
                 popup = paste("No. of bicycle thefts: ", thefts))
  })
  
  observeEvent(input$button,{
    leafletProxy("safetymap")%>%clearShapes()
  })
})