#Load required library packages
library(httr)
library(jsonlite)
library(xml2)
library(dplyr)
library(lubridate)
library(tidyr)
library(data.table)
library(purrr)
library(magrittr)
library(ggplot2)
library(readr)

#Turn off (FALSE) R Studio feature of turning character strings automatically into factor variables
options(stringsAsFactors = TRUE)

#Setting working directory
setwd("/Users/Blae/Documents/Work/Data Science")

#Using for looping index to obtain monthly datasets from 2011-2016

crime <- list()
k <- 1
for(i in 2011:2016){
  for(j in 1:12){
    searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.514,-0.225:51.484,-0.225:51.484,-0.105:51.514,-0.105&date=",i),paste0("-",j))
    cr <- GET(searchurl)
    cc <- content(cr, as="text",encoding = "UTF-8")
    ct <- cc %>% fromJSON()
    crime[[k]] <- ct
    k <- k+1
  }
}
#Using for looping index to obtain 2017 datasets
crime2017 <- list()
k <- 1
for(i in 1:3){
    searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.514,-0.225:51.484,-0.225:51.484,-0.105:51.514,-0.105&date=2017-",i))
    cr <- GET(searchurl)
    cc <- content(cr, as="text",encoding = "UTF-8")
    ct <- cc %>% fromJSON()
    crime2017[[k]] <- ct
    k <- k+1
  }


#Script from Dhiraj using Purrr function to convert lists to dataframes, can't use it on all
#the variables as dataframes are nested within lists for location and outcome_status
crime2011to16 <- map_df(crime, extract, c("category","location_type","context",
                                      "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(crime, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(crime, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(crime, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(crime, ~.x$outcome_status[["date"]]))

#Use cbind to combine columns together
crimedb <- cbind(crime2011to16,latitude,longitude,outcome_status_category,outcome_status_date)


crime2017full <- map_df(crime2017, extract, c("category","location_type","context",
                                            "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(crime2017, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(crime2017, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(crime2017, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(crime2017, ~.x$outcome_status[["date"]]))

crime2017full <- cbind(crime2017full, latitude, longitude, outcome_status_category, outcome_status_date)

#Use rbind to combine rows together
crime11to17 <- rbind(crimedb, crime2017full)

saveRDS(crime11to17, "crime11to17.rds")
saveRDS(crime2011to16, "crime11to16.rds")
saveRDS(crime2017full, "crime17.rds")


