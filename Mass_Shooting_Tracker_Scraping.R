# Web Scraping from Mass Shooting Tracker

# Load Applicable Libraries ====================================================
library(shiny)
library(tidyverse)
library(rvest)
library(purrr)
library(lubridate)
library(RColorBrewer)




dfmass <- lapply(paste0("https://www.massshootingtracker.org/data/all"),
                 function(url){
                   url %>% read_html() %>% 
                     html_nodes("tr td") %>% 
                     html_text()
                 })

dfmass <- unlist(dfmass)
dfmass <- data.frame(
  incident_id = dfmass[seq(from = 1, to = length(dfmass), by = 7)],
  incident_date = dfmass[seq(from = 2, to = length(dfmass), by = 7)],
  Location = dfmass[seq(from = 3, to = length(dfmass), by = 7)],
  no.killed = dfmass[seq(from = 4, to = length(dfmass), by = 7)],
  no.injured = dfmass[seq(from = 5, to = length(dfmass), by = 7)],
  shooters = dfmass[seq(from = 6, to = length(dfmass), by = 7)]
)

dfmass[1] <- NULL

dfmass$city_county <- lapply(strsplit(as.character(dfmass$Location), split = ","),"[[",1)
dfmass$state <- lapply(strsplit(as.character(dfmass$Location), split = ","),"[[",2)
dfmass$date <- mdy(as.character(dfmass$incident_date))
dfmass$city_county <- as.character(dfmass$city_county)
dfmass$state <- as.character(dfmass$state)
dfmass$no.injured <- as.integer(dfmass$no.injured)
dfmass$no.killed <- as.integer(dfmass$no.killed)

dfmass %>% 
  select(date, no.killed) %>% 
  group_by(Date = floor_date(x = date, unit = "month")) %>% 
  tally(name = "Number of Deaths") %>% 
  ggplot(aes(x = Date, y = `Number of Deaths`)) + geom_col(aes(fill = `Number of Deaths`)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  ggtitle("Number of Gun Related Deaths in Mass Shootings Over Time")

