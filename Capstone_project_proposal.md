Capstone Project Proposal
=========================

Idea \#3 has been chosen to be the focus of my Capstone project, focussing on street crime in London.

Problem Definition
==================

There is no easily accessible way for safety conscious Londonders to check whether an area is a hotspot for a certain type of crime. This may be in the context of checking muggings before heading somewhere on a night out or for instances of bicycle theft to identify where it is relatively safe to leave their bicycle. This may also be useful for tourists visiting the city to identify whether they are in a higher risk area. Are there correlations between certain areas and certain crimes? Are there

Client Description and Requirements
===================================

The Metropolitan Police operate across 32 boroughs within Greater London and aim to make London the safest global city. To this end, an accessible method allowing locals to check where crime hotspots are located would allow them to make informed decisions about where they go and encourage preventative action. This would be a useful service for the Met Police to promote with the aim of reducing opportunistic crimes by raising awareness.

Data Availability and Acquisition
=================================

Datasets for street level crime in England, Wales and Northern Ireland are available at (<https://data.police.uk/docs/method/crime-street/>). Data is reported monthly and is available from December 2010 to March 2017 at present. All the data on this site is made available under the Open Government License v3.0.

There is an associated API which can efficiently and rapidly provide a rich data source of information. The API is implemented as a standard JSON web service using HTTP GET and POST requests.

**Example API Request** <https://data.police.uk/api/crimes-street/all-crime?poly=52.268,0.543:52.794,0.238:52.130,0.478&date=2013-01>

To keep the size of the dataset manageable an area over central London has been defined. The *poly* parameter identifies the polygon within which data is extracted and is formatted in lat/lng pairs, separated by colons:

poly=\[lat\],\[lng\]:\[lat\],\[lng\]:\[lat\],\[lng\]:\[lat\],\[lng\]

The date also needs to be specified or the most recent month will be returned. The limit of a custom search area is 10,000 crimes, if it contains more than this the API will return a 503 status code.

The following variables are returned by the street-level crime API: category, location\_type, context, persistent\_id, id, location\_subtype, month, latitude, longitude, outcome\_status\_category, outcome\_status\_data.

Location data is only referencing the approximate location using 'anonymous' map points. This is to protect the privacy of victim. The date of occurence is also truncated to record only month and year for the same reason.

The API will be used to acquire the full dataset from December 2011 to March 2017 for central London.

Approach
========

The main steps which will be undertaken for this project are: *Data acquistion - using API to obtain data from (<http://data.police.uk>), identify suitable area to use which does not exceed call limit of 10,000. *Data wrangling - combine all months together into one data frame initially, this will require unnesting data frames sitting within main data frame. Split month and year into separate variables, simplify names of columns, determine how to deal with missing values. *Exploratory data analysis - using a combination of inferential statistics and data visualisation determine hot spots for crime in central London, identify seasonal or geographic pattern in crime types and whether their relative frequency is changing over time. *Shiny App - map based visualisation of crime hotspots using Kernel Density Estimation. \*Machine learning - if deemed appropriate

Deliverables
============

The key deliverables for the project will be uploaded to GitHub, they include: *Code for the project with annotation included *Final report explaining the problem, the approach and findings - included ideas for further research, as well as up to three ideas on how to use the findings *Slide pack covering the same material as the final report *Present the project to the online community either in an office hour, online video or blog post
