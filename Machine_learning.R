#Analysis 4 is focussing on creating a linear regression model using the inner borough dataset
library(dplyr)
library(plyr)
library(lubridate)
library(zoo)

setwd("/Users/Blae/Documents/Work/Data Science/Capstone_Project/")
crime16 <- readRDS("crime16.rds")

#Convert the factor variable date to a date variable, convert month number to characters
crime16$date <- as.yearmon(paste(crime16$year, crime16$month, sep = "-")) 
crime16$date <- dmy(paste("01", crime16$date, sep = " "))
crime16$month <- month.name[crime16$month]
glimpse(crime16)


crime16$crime_type <- as.character(crime16$crime_type)
table(crime16$crime_type)


#Modeling engine would need to be constructed at a reasonably sized predefined geographical area for reasonably long predefined time interval
#Crime counts for each borough for each month:
crime.agg <- ddply(crime16, .(crime_type, borough, date), summarize, count = length(date), .progress = "text")

crime.agg$month <- months(as.Date(crime.agg$date), abbreviate = TRUE)
crime.agg$borough <- as.character(crime.agg$borough)

#Determine history of criminal activity in past - use ave() and supplement with customised input function
#Look back in time any number of months by supplying one argument

pastMonths <- function(x) {
  c(0, rep(1, x)) 
  }
crime.agg$past.crime.1 <- ave(crime.agg$count, crime.agg$borough, FUN = function(x) filter(x, pastMonths(1), sides = 1))

crime.agg$season <- as.factor(ifelse(crime.agg$month %in% c("Mar", "Apr", "May"), "spring",
                                        ifelse(crime.agg$month %in% c("Jun", "Jul", "Aug"), "summer",
                                               ifelse(crime.agg$month %in% c("Sep", "Oct", "Nov"), "autumn", "winter"))))

#Load psych to determine correlation between dependent and independent variables
#count is the dependent variable, it is discrete and counts the number of a certain category of crime in a certain borough, in a specific month
library(psych)
#Can only use numeric inputs - need to calculate past 3 and past 6 months
model.cor <- cor(crime.agg[, c("count", "season", "month")]) 
cor.plot(model.cor)

mean(crime.agg$count)
var(crime.agg$count)

#Variance is much greater than mean, distibution is overdispersed
#A suitable way to model overdispersion is using the negative binomial distribution






##########DRAFT################

length(unique(crime.agg$borough))
length(unique(crime.agg$date))
boroughs <- sort(unique(crime.agg$borough))
dates <- sort(as.character(unique(crime.agg$date)))
temp <- expand.grid(boroughs, dates)
names(temp) <- c("borough", "date")
temp <- sort_by(~borough, data = temp)
model.data <- aggregate(crime.agg[, c("count")], by = list(crime.agg$borough, as.character(crime.agg$date)), FUN = sum)
names(model.data) <- c("borough", "date", "count")
