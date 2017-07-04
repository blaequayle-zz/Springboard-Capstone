library(shiny)
library(leaflet)

shinyUI(navbarPage(title=div(img(src="skyline.png", width="27%", align = "right"),"London Bicycle Theft Analysis"),
                       tabPanel("Location of Bicycle Thefts",
                            sidebarLayout(
                              sidebarPanel(
                                
                                dateRangeInput('dateRange',
                                               label = 'Filter records between dates:',
                                               start = min(bike16$date), end = max(bike16$date) 
                                ),
                                width=3
                              ),
                              mainPanel(
                                p("The plot below shows the thefts of bicycles in the 12 inner London boroughs for 2016.
                                  The data has been obtained from the open access site http://data.police.uk.
                                  The sidebar panel on the left allows the user to select a data range."),
                                leafletOutput("popmap", width="100%", height=800)
                              )
                            )
                   ),
                   tabPanel("Bicycle Heat Map",
                            sidebarLayout(
                            sidebarPanel(
                              dateRangeInput('dateRange1',
                                             label = 'Filter records between dates:',
                                             start = min(bike16$date), end = max(bike16$date)
                              ),

                              width=3
                            ),
                              mainPanel(
                                p("This heatmap allows the user to identify hotspots for bicycle theft.
                                  Specific date ranges can be selected from the sidebar panel to filter out records for a particular time period."),
                                leafletOutput("heatmap", width="100%",height=800)
                              )
                            )     
                   ),
                   tabPanel("How Safe Is This?",
                            mainPanel(
                                h2("Click On Map to Get Safety Zone"),
                                
                                p("The app calculates the number of bicycle thefts that have taken place in a radius of 1 Km from 
                                  the point that has been clicked on the map. If less than 200 thefts have occurred the circle is green.
                                  Between 200 and 400 thefts, the circle is coloured orange and beyond 400, the circle is red.
                                  The Met Police can encourage the users of this app to be aware of where they choose to lock their bikes."),
                                actionButton("button","Clear Circles"),
                                leafletOutput("safetymap", width = "100%",height=800)
                              )
                   )
                   
))