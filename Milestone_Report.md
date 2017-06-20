Capstone Milestone Report
=========================

1. Introduction
---------------

The Metropolitan Police operate across 32 boroughs within Greater London and aim to make London the safest global city. To this end, an accessible method allowing both residents and tourists to check where crime hotspots are located would allow them to make informed decisions about where they go and encourage preventative action. This would be a useful service for the Met Police to promote with the aim of reducing opportunistic crimes by raising awareness of the highest risk areas.

For locals, this may be in the context of checking incidences of bicycle theft to identify where is best to lock up your bike, or hotspots for burglaries when looking to rent or purchase property. This may also be useful for tourists visiting the city to flag if they are in a higher risk area.

This project aims to address several key questions: 1.Is crime on the increase or decrease in central London? 2.Is the frequency of specific crimes reducing or increasing on an annual basis? 3.Is there any seasonal variation in the frequency of crimes? 4.Are certain crimes more prevalent in certain areas - where are these 'hotspots'? 5.Create a visualisation allowing easy identification of risk level to a certain crime 6.Can correlations be made between crime occurrences and other indicators e.g. employment levels? 7. Can time series forecasting be used to estimate occurence of crime in the future?

2. Data acquisition
-------------------

Datasets for street level crime in England, Wales and Northern Ireland are available at (<https://data.police.uk/docs/method/crime-street/>). Data is reported monthly and is available from December 2010 to March 2017 at present. All the data on this site is made available under the Open Government License v3.0.

There is an associated API which can efficiently and rapidly provide a rich data source of information. The API is implemented as a standard JSON web service using HTTP GET and POST requests.

**Example API Request** <https://data.police.uk/api/crimes-street/all-crime?poly=52.268,0.543:52.794,0.238:52.130,0.478&date=2013-01>

To keep the size of the dataset manageable an area over central London has been defined. The *poly* parameter identifies the polygon within which data is extracted and is formatted in lat/lng pairs, separated by colons:

poly=\[lat\],\[lng\]:\[lat\],\[lng\]:\[lat\],\[lng\]:\[lat\],\[lng\]

The date also needs to be specified or the most recent month will be returned. The limit of a custom search area is 10,000 crimes, if it contains more than this the API will return a 503 status code.

Location data is only referencing the approximate location using 'anonymous' map points. This is to protect the privacy of victim. The date of occurence is also truncated to record only month and year for the same reason.

The API will initially be used to acquire the full dataset from December 2011 to March 2017 for central London as defined by a coordinates box with corners: 51.514,-0.225 51.484,-0.225 51.484,-0.105 51.514,-0.105

3. Important fields and information
-----------------------------------

Variable names, types and number of observations were reviewed using glimpse(crime11to17). This reveals there are 11 variables - 6 characters, 3 numerics (1 integer, 2 double precision) and 2 factors. There are 498,628 observations in total.

The following variables are returned: **category**, location\_type, context, persistent\_id, id, location\_subtype, **month**, **latitude**, **longitude**, outcome\_status\_category, outcome\_status\_data. The key fields for this study are in **bold**.

There are 15 different types of crime (variable category). Month and year are available but not day due to the police anonymisation process. Latitude and longitude are provided. Location data is only referencing the approximate location using 'anonymous' map points.

4.Data limitations
------------------

The main limitations that have been identified with the dataset are: \* Anonymistation of the data means the locations are fixed to a 'map point' and jittering is required to make all the events visible, although as the dataset gets larger it is not possible to create an effective map visualisation of individual crimes. \* Anonymisation also means no day/time is provided so it is not possible to analyse potential relationships with certain crimes. \* It is difficult at present to integrate other datasets relating to indicators at a Borough level as the data is downloaded using the API over a polygon area of central London which encompasses parts of Westminster, Camden, Lambeth and Southwark.

5. Data cleaning and wrangling
------------------------------

The following packages were loaded in preparation for data wrangling: readr, dplyr, tidyr and lubridate.

They key data wrangling steps that were undertaken as part of this project were as follows:

-   Convert category and location\_type to factor using as.factor().

-   Several of the variables contained no useful data for a civilian user. The variables context, persistent\_id, id and location\_subtype were removed with select() and variable names for category and location\_type were changed to crime\_type and service respectively, to make the dataset clearer using rename().

-   Checked columns for missing values using sapply(crime11to17, function(x) sum(is.na(x))). Only the outcome\_status\_category and outcome\_status\_date contain NA values and there is no plan to use these in the scope of the current project.

-   Use the lubridate functions year() and month() to create new columns called year and month from the original 'month' variable which was in format y-m.

-   Create dataframe for monthly crime statistics from 2011 to 2017. Abbreviate the crime\_type into new variable called crime\_abb to improve data visualisations using case\_when() function nested within mutate(). The 'r group\_by()' function is then used to sort by year, month and crime\_abb. summarize() is used to get a count for each of the new groupings.

-   Repeat process for annual crime statistics from 2011 to 2016, 2017 removed as only a partial year.

-   Create dataframe for seasonal crime numbers using case\_when() to assign Spring (March to May), Summer (June to August), Autumn (September to November) or Winter (December to February) to new season variable. Removed Mar-2017 from analysis as not sufficient data to constitute a full season.

-   Save dataframes created using saveRDS for loading into analysis section.

6. Preliminary exploration
--------------------------

The most prevalent crime in the 2011-2017 period is anti-social-behaviour. This accounts for 23% of total recorded crime. The majority of the records are from the Police Force but a small portion is submitted by the British Transport Police who operate on the railways. These records account for only 4.3%. It would be interesting to explore the annual variation in street crimes using a bar chart. The total crimes per year are calculated using group\_by(year) and summarize(Count=n()).

![](Milestone_Report_files/figure-markdown_github/unnamed-chunk-2-1.png)

The abbreviations used are as follows:

| Abbreviation | Crime                   |
|--------------|-------------------------|
| ASB          | Anti-social behaviour   |
| BT           | Bicycle theft           |
| BU           | Burglary                |
| CDA          | Criminal damage & arson |
| DR           | Drugs                   |
| OC           | Other crime             |
| OT           | Other theft             |
| PDW          | Public disorder weapons |
| PO           | Public order            |
| PoW          | Possesion of weapons    |
| RO           | Robbery                 |
| SL           | Shoplifting             |
| TP           | Theft from the person   |
| VEC          | Vehicle crime           |
| VIC          | Violent crime           |

Recorded annual street crime has been decreasing each year in London since 2011. There was a significant decrease between 2012 and 2013 of 11.8% and subsequent years show a reduction with a less steep negative gradient. The variation of specific crime frequency over time is displayed using a bar chart and a line plot with the annual.crime dataframe as input.

![](Milestone_Report_files/figure-markdown_github/unnamed-chunk-3-1.png)

![](Milestone_Report_files/figure-markdown_github/unnamed-chunk-4-1.png)

Three categories have shown a strong decrease between 2011 and 2016 - other crime, other theft and anti-social behaviour. The anonymously large drop between 2011 and 2012 for other crime, and large increase of other theft between 2012 and 2013 may suggest a change in recording/reporting. Worryingly, violent crime displays a steady increase from 2013 onwards.

The monthly.crime dataframe is used to investigate time series. Initially, the date column needs to be set to the correct format using Lubridate.

A time series is plotted using geom\_area() which stacks time series at each point. The colour scale for the crime categories is manually set using a vector due to the large number of categories.

![](Milestone_Report_files/figure-markdown_github/unnamed-chunk-7-1.png)

This illustrates the changing classification of crimes over time and suggests there was a drive to classify 'other crimes' into different categories. 'Other theft' did not exist prior to late 2011 and seems to increase by a similar degree to the decrease in 'other crime'. Several other categories are added at this point: drugs, shoplifting, criminal damage & arson, and public disorder weapons. The final tranche of categories are introduced in early 2013 - public order, bicycle theft and theft from the person. By early 2017, the other-crime category is used only minimally.

A time series is plotted using geom\_bar() which suggests anti-social behaviour peaks each summer, but due to the scale used it is difficult to analyse it further.

![](Milestone_Report_files/figure-markdown_github/unnamed-chunk-8-1.png)

A heatmap can be used to analyse seasonal variation across all crime categories.

![](Milestone_Report_files/figure-markdown_github/unnamed-chunk-9-1.png)

This clearly demonstrates the seasonal fluctuations in anti-social behaviour, with peaks each summer. Bicycle crime and violent crime also appear to be more common in May-July. Most other crimes show minimal seasonal variation.

These variations will be analysed in more detail using the seasonal.crime dataframe. Seasonal colours are set using a vector so the line segments can be individually coloured. Date is converted for time series use. A line plot is created with vertical lines to indicate the day with the highest temperature (red lines). This is created using a dataframe with the maximum London temperature for each year (hottest.day).

![](Milestone_Report_files/figure-markdown_github/unnamed-chunk-11-1.png)

There has been a clear decrease in anti-social behaviour over time, with the exception of 2016 which showed a significant increase. Clear peaks are visible in the summer (orange segments), often coinciding with August when the school summer holidays occur. The peaks frequently coincide very closely to the hottest day of the year, with the exception of 2014. Minimum occurences are consistently just after 1st January. This process will be repeated for bicycle theft and burglaries.

![](Milestone_Report_files/figure-markdown_github/unnamed-chunk-12-1.png)

![](Milestone_Report_files/figure-markdown_github/unnamed-chunk-13-1.png)

Bicycle theft statistics are only available from 2013 and they have been fairly steady from 2014 onwards. The time series is cyclical with peaks generally correspond reasonably (within 3 weeks) to the hottest day of the year, and minimum recorded incidences always occuring in the winter.

Burglaries have generally decreased since 2012, though there was a surge in Spring 2012 and another peak in Autumn 2016. There is less of a pattern visible in burglary occurences, though there is frequently a peak in the run up to Christmas followed by a sharp decrease over New Year. There is no clear pattern between high temperatures and burglary.

The Leaflet package can be used to visualise the location of crime occurences. A reduced dataset is required and is only useful if the data is filtered to a single year and single crime, due to overlapping or proximal datapoints. Jittering could be used to improve this.

It would be valuable to be able to visualise the density of crimes rather than individual occurences. A dataframe is created from the original dataset containing only bicycle theft. Jitter is then added to the latitude and longitude. Leaflet can be used to visualise the output. An improvement is visible but it's still difficult to interpret.

Kernel Density Estimation (KDE) is a way to estimate the probability density function of a random variable, inferences about the population are made based on a finite data sample. The latitude and longitude for bicycle thefts are saved in a separate dataframe. KDE is carried out using bkde2D() with the df, bandwidth and gridsize as input. The contour lines created are then converted to spatial polygons.

``` r
latlong <- select(bike, longitude, latitude)
kde <- bkde2D(latlong,
              bandwidth=c(.0055, .0048), gridsize = c(75,75))
cl <- contourLines(kde$x1 , kde$x2 , kde$fhat)
levs <- as.factor(sapply(cl, `[[`, "level"))
nlev <- length(levels(levs))
pgons <- lapply(1:length(cl), function(i) Polygons(list(Polygon(cbind(CL[[i]]$x, CL[[i]]$y))), ID=i))
spgons <- SpatialPolygons(pgons)
leaflet(spgons) %>% addTiles() %>%
  addPolygons(color = heat.colors(nlev, NULL)[levs])
```

The heat map shows a concentration of bicycle thefts in Covent Garden and on the South Bank, two of the most popular tourist areas in London.

7. Initial findings and impact on approach
------------------------------------------

Crime rates have been dropping consistently in London from 2011 onwards. However, certain crimes have shown an increase over that time e.g. violent crime. There are seasonal patterns in crime present, as evidenced by peaks of anti-social behaviour correlating to the temperature peaks in the summer season. Bicycle theft is also clearly cyclical.

The dataset would be too large if all London boroughs were incorporated into the analysis. It is proposed to use shapefiles of the boroughs outlines to sort the data to include only the 'inner' London boroughs. These are Camden, Greenwich, Hackney, Hammersmith and Fulham, Islington, Kensington and Chelsea, Lambeth, Lewisham, Southwark, Tower Hamlets, Wandsworth and Westminster. This would allow investigation into geographic patterns plus correlations with other indicators available on a borough level.

This would also enable the normalisation of crime rates, which would go some way towards removing the effect of a high population density within boroughs. This can be done by dividing total crime number by the borough population. An additional dataset will be loaded which includes population plus other key economic indicators such as employment, on a borough level.

Kernel Density Estimation maps will be updated using the inner boroughs dataset, providing a far more extensive overview of high risk areas. Chloropleth maps will be created to interactively display the frequency of specific crimes across boroughs. A combination of both these interactive maps could be used to populate a Shiney App.
