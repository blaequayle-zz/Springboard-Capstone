Capstone Project
----------------

Data Wrangling
==============

The following packages were loaded in preparation for data wrangling: readr, dplyr, tidyr and lubridate.

They key data wrangling steps that were undertaken as part of this project were as follows:

-   Use glimpse(crime11to17) to review variable names, variable types, and number of observations. This reveals there are 11 variables - 6 characters, 3 numerics (1 integer, 2 double precision) and 2 factors. There are 498,628 observations in total.

-   Convert category and location\_type to factor using as.factor().

-   Several of the variables contained no useful data for a civilian user. The variables context, persistent\_id, id and location\_subtype were removed with select() and variable names for category and location\_type were changed to crime\_type and service respectively, to make the dataset clearer using rename().

-   Checked columns for missing values using sapply(crime11to17, function(x) sum(is.na(x))). Only the outcome\_status\_category and outcome\_status\_date contain NA values and there is no plan to use these in the scope of the current project.

-   Use the lubridate functions year() and month() to create new columns called year and month from the original 'month' variable which was in format y-m.

-   Create dataframe for monthly crime statistics from 2011 to 2017. Abbreviate the crime\_type into new variable called crime\_abb to improve data visualisations using case\_when() function nested within mutate(). The 'r group\_by()' function is then used to sort by year, month and crime\_abb. summarize() is used to get a count for each of the new groupings.

-   Repeat process for annual crime statistics from 2011 to 2016, 2017 removed as only a partial year.

-   Create dataframe for seasonal crime numbers using case\_when() to assign Spring (March to May), Summer (June to August), Autumn (September to November) or Winter (December to February) to new season variable. Removed Mar-2017 from analysis as not sufficient data to constitute a full season.

-   Save dataframes created using saveRDS for loading into analysis section.
