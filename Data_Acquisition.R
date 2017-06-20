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

#######Inner London 2016########
#Call limit is 10,000 so each polygon for location needs to return less than 10,000 records per month.
#Using for looping index to obtain 2016 datasets for all inner London boroughs

central2016A <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.611,-0.291:51.560,-0.291:51.560,-0.182:51.611,-0.182&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016A[[k]] <- ct
  k <- k+1
}

central2016Afull <- map_df(central2016A, extract, c("category","location_type","context",
                                              "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016A, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016A, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016A, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016A, ~.x$outcome_status[["date"]]))

central2016A <- cbind(central2016Afull, latitude, longitude, outcome_status_category, outcome_status_date)
###############################
central2016B <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.611,-0.182:51.560,-0.182:51.560,-0.072:51.611,-0.072&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016B[[k]] <- ct
  k <- k+1
}

central2016Bfull <- map_df(central2016B, extract, c("category","location_type","context",
                                               "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016B, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016B, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016B, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016B, ~.x$outcome_status[["date"]]))

central2016B <- cbind(central2016Bfull, latitude, longitude, outcome_status_category, outcome_status_date)
#########################
central2016C <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.611,-0.072:51.560,-0.072:51.560,0.038:51.611,0.038&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016C[[k]] <- ct
  k <- k+1
}

central2016Cfull <- map_df(central2016C, extract, c("category","location_type","context",
                                               "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016C, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016C, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016C, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016C, ~.x$outcome_status[["date"]]))

central2016C <- cbind(central2016Cfull, latitude, longitude, outcome_status_category, outcome_status_date)
########################
central2016D <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.611,0.038:51.560,0.038:51.560,0.148:51.611,0.148&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016D[[k]] <- ct
  k <- k+1
}

central2016Dfull <- map_df(central2016D, extract, c("category","location_type","context",
                                               "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016D, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016D, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016D, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016D, ~.x$outcome_status[["date"]]))

central2016D <- cbind(central2016Dfull, latitude, longitude, outcome_status_category, outcome_status_date)
#######################
central2016E <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.560,-0.291:51.510,-0.291:51.510,-0.182:51.560,-0.182&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016E[[k]] <- ct
  k <- k+1
}

central2016Efull <- map_df(central2016E, extract, c("category","location_type","context",
                                               "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016E, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016E, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016E, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016E, ~.x$outcome_status[["date"]]))

central2016E <- cbind(central2016Efull, latitude, longitude, outcome_status_category, outcome_status_date)
########################
central2016F1 <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.560,-0.182:51.535,-0.182:51.535,-0.072:51.560,-0.072&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016F1[[k]] <- ct
  k <- k+1
}

central2016F1full <- map_df(central2016F1, extract, c("category","location_type","context",
                                               "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016F1, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016F1, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016F1, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016F1, ~.x$outcome_status[["date"]]))

central2016F1 <- cbind(central2016F1full, latitude, longitude, outcome_status_category, outcome_status_date)
##########################
central2016F2 <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.535,-0.182:51.510,-0.182:51.510,-0.072:51.535,-0.072&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016F2[[k]] <- ct
  k <- k+1
}

central2016F2full <- map_df(central2016F2, extract, c("category","location_type","context",
                                                      "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016F2, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016F2, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016F2, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016F2, ~.x$outcome_status[["date"]]))

central2016F2 <- cbind(central2016F2full, latitude, longitude, outcome_status_category, outcome_status_date)

############

central2016G <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.560,-0.072:51.510,-0.072:51.510,0.038:51.560,0.038&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016G[[k]] <- ct
  k <- k+1
}

central2016Gfull <- map_df(central2016G, extract, c("category","location_type","context",
                                               "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016G, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016G, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016G, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016G, ~.x$outcome_status[["date"]]))

central2016G <- cbind(central2016Gfull, latitude, longitude, outcome_status_category, outcome_status_date)
########################
central2016H <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.560,0.038:51.510,0.038:51.510,0.148:51.560,0.148&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016H[[k]] <- ct
  k <- k+1
}

central2016Hfull <- map_df(central2016H, extract, c("category","location_type","context",
                                               "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016H, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016H, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016H, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016H, ~.x$outcome_status[["date"]]))

central2016H <- cbind(central2016Hfull, latitude, longitude, outcome_status_category, outcome_status_date)

##################
central2016I <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.510,-0.291:51.460,-0.291:51.460,-0.182:51.510,-0.182&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016I[[k]] <- ct
  k <- k+1
}

central2016Ifull <- map_df(central2016I, extract, c("category","location_type","context",
                                                    "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016I, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016I, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016I, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016I, ~.x$outcome_status[["date"]]))

central2016I <- cbind(central2016Ifull, latitude, longitude, outcome_status_category, outcome_status_date)
#################
central2016J <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.510,-0.182:51.460,-0.182:51.460,-0.072:51.510,-0.072&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016J[[k]] <- ct
  k <- k+1
}

central2016Jfull <- map_df(central2016J, extract, c("category","location_type","context",
                                                    "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016J, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016J, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016J, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016J, ~.x$outcome_status[["date"]]))

central2016J <- cbind(central2016Jfull, latitude, longitude, outcome_status_category, outcome_status_date)
#################

central2016K <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.510,-0.072:51.460,-0.072:51.460,0.038:51.510,0.038&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016K[[k]] <- ct
  k <- k+1
}

central2016Kfull <- map_df(central2016K, extract, c("category","location_type","context",
                                                    "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016K, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016K, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016K, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016K, ~.x$outcome_status[["date"]]))

central2016K <- cbind(central2016Kfull, latitude, longitude, outcome_status_category, outcome_status_date)
##################

central2016L <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.510,0.038:51.460,0.038:51.460,0.148:51.510,0.148&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016L[[k]] <- ct
  k <- k+1
}

central2016Lfull <- map_df(central2016L, extract, c("category","location_type","context",
                                                    "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016L, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016L, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016L, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016L, ~.x$outcome_status[["date"]]))

central2016L <- cbind(central2016Lfull, latitude, longitude, outcome_status_category, outcome_status_date)
#################

central2016M <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.460,-0.291:51.410,-0.291:51.410,-0.182:51.460,-0.182&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016M[[k]] <- ct
  k <- k+1
}

central2016Mfull <- map_df(central2016M, extract, c("category","location_type","context",
                                                    "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016M, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016M, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016M, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016M, ~.x$outcome_status[["date"]]))

central2016M <- cbind(central2016Mfull, latitude, longitude, outcome_status_category, outcome_status_date)
##################
central2016N <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.460,-0.182:51.410,-0.182:51.410,-0.072:51.460,-0.072&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016N[[k]] <- ct
  k <- k+1
}

central2016Nfull <- map_df(central2016N, extract, c("category","location_type","context",
                                                    "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016N, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016N, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016N, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016N, ~.x$outcome_status[["date"]]))

central2016N <- cbind(central2016Nfull, latitude, longitude, outcome_status_category, outcome_status_date)

##################
central2016O <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.460,-0.072:51.410,-0.072:51.410,0.038:51.460,0.038&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016O[[k]] <- ct
  k <- k+1
}

central2016Ofull <- map_df(central2016O, extract, c("category","location_type","context",
                                                    "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016O, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016O, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016O, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016O, ~.x$outcome_status[["date"]]))

central2016O <- cbind(central2016Ofull, latitude, longitude, outcome_status_category, outcome_status_date)
##################
central2016P <- list()
k <- 1
for(i in 1:12){
  searchurl <- paste0(paste0("https://data.police.uk/api/crimes-street/all-crime?poly=51.460,0.038:51.410,0.038:51.410,0.148:51.460,0.148&date=2016-",i))
  cr <- GET(searchurl)
  cc <- content(cr, as="text",encoding = "UTF-8")
  ct <- cc %>% fromJSON()
  central2016P[[k]] <- ct
  k <- k+1
}

central2016Pfull <- map_df(central2016P, extract, c("category","location_type","context",
                                                    "persistent_id","id","location_subtype","month"))

latitude <- as.numeric(unlist(map(central2016P, ~.x$location[["latitude"]])))
longitude <- as.numeric(unlist(map(central2016P, ~.x$location[["longitude"]])))
outcome_status_category <- unlist(map(central2016P, ~.x$outcome_status[["category"]]))
outcome_status_date <- unlist(map(central2016P, ~.x$outcome_status[["date"]]))

central2016P <- cbind(central2016Pfull, latitude, longitude, outcome_status_category, outcome_status_date)

##################
central2016 <- rbind(central2016A, central2016B, central2016C, central2016D, central2016E, central2016F1, central2016F2, central2016G, central2016H, central2016I, central2016J, central2016K, central2016L, central2016M, central2016N, central2016O, central2016P)
saveRDS(central2016, "central16.rds")