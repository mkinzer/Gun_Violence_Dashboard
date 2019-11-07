# Load Applicable Libraries ====================================================
library(shiny)
library(tidyverse)
library(rvest)
library(purrr)
library(lubridate)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(highcharter)

# Year Frames===================================================================
# Extract all HTML nodes and assign to a dataframe for that year. To save space,
# column names are not labelled per year, so the following code MUST be run in
# order.

# Note that for Years 2016 on there is no incident ID column which must be
# dropped form the rest once combined

# 2013 Data ====================================================================
df2013 <- lapply(paste0("https://www.gunviolencearchive.org/reports/mass-shootings/2013?page=", 0:11),
       function(url){
         url %>% read_html() %>% 
           html_nodes("tr td") %>% 
           html_text()
       })
df2013 <- unlist(df2013)
df2013 <- data.frame(
  incident_id = df2013[seq(from = 1, to = length(df2013), by = 8)],
  incident_date = df2013[seq(from = 2, to = length(df2013), by = 8)],
  state = df2013[seq(from = 3, to = length(df2013), by = 8)],
  city_county = df2013[seq(from = 4, to = length(df2013), by = 8)],
  address = df2013[seq(from = 5, to = length(df2013), by = 8)],
  no.killed = df2013[seq(from = 6, to = length(df2013), by = 8)],
  no.injured = df2013[seq(from = 7, to = length(df2013), by = 8)]
)

df2013[1] <- NULL

# 2014 Data ====================================================================
df2014 <- lapply(paste0("https://www.gunviolencearchive.org/reports/mass-shootings/2014?page=", 0:11),
                 function(url){
                   url %>% read_html() %>% 
                     html_nodes("tr td") %>% 
                     html_text()
                 })
df2014 <- unlist(df2014)
df2014 <- data.frame(
  incident_id = df2014[seq(from = 1, to = length(df2014), by = 8)],
  incident_date = df2014[seq(from = 2, to = length(df2014), by = 8)],
  state = df2014[seq(from = 3, to = length(df2014), by = 8)],
  city_county = df2014[seq(from = 4, to = length(df2014), by = 8)],
  address = df2014[seq(from = 5, to = length(df2014), by = 8)],
  no.killed = df2014[seq(from = 6, to = length(df2014), by = 8)],
  no.injured = df2014[seq(from = 7, to = length(df2014), by = 8)]
)

df2014[1] <- NULL

# 2015 Data ====================================================================
df2015 <- lapply(paste0("https://www.gunviolencearchive.org/reports/mass-shootings/2015?page=", 0:14),
                 function(url){
                   url %>% read_html() %>% 
                     html_nodes("tr td") %>% 
                     html_text()
                 })
df2015 <- unlist(df2015)
df2015 <- data.frame(
  incident_id = df2015[seq(from = 1, to = length(df2015), by = 8)],
  incident_date = df2015[seq(from = 2, to = length(df2015), by = 8)],
  state = df2015[seq(from = 3, to = length(df2015), by = 8)],
  city_county = df2015[seq(from = 4, to = length(df2015), by = 8)],
  address = df2015[seq(from = 5, to = length(df2015), by = 8)],
  no.killed = df2015[seq(from = 6, to = length(df2015), by = 8)],
  no.injured = df2015[seq(from = 7, to = length(df2015), by = 8)]
)

df2015[1] <- NULL

# 2016 Data ====================================================================
df2016 <- lapply(paste0("https://www.gunviolencearchive.org/reports/mass-shooting?year=2016&page=", 0:16),
                 function(url){
                   url %>% read_html() %>% 
                     html_nodes("tr td") %>% 
                     html_text()
                 })
df2016 <- unlist(df2016)
df2016 <- data.frame(
  #incident_id = df2016[seq(from = 1, to = length(df2016), by = 8)],
  incident_date = df2016[seq(from = 1, to = length(df2016), by = 7)],
  state = df2016[seq(from = 2, to = length(df2016), by = 7)],
  city_county = df2016[seq(from = 3, to = length(df2016), by = 7)],
  address = df2016[seq(from = 4, to = length(df2016), by = 7)],
  no.killed = df2016[seq(from = 5, to = length(df2016), by = 7)],
  no.injured = df2016[seq(from = 6, to = length(df2016), by = 7)]
)

# 2017 Data ====================================================================
df2017 <- lapply(paste0("https://www.gunviolencearchive.org/reports/mass-shooting?year=2017&page=", 0:14),
                 function(url){
                   url %>% read_html() %>% 
                     html_nodes("tr td") %>% 
                     html_text()
                 })
df2017 <- unlist(df2017)
df2017 <- data.frame(
  #incident_id = df2017[seq(from = 1, to = length(df2017), by = 8)],
  incident_date = df2017[seq(from = 1, to = length(df2017), by = 7)],
  state = df2017[seq(from = 2, to = length(df2017), by = 7)],
  city_county = df2017[seq(from = 3, to = length(df2017), by = 7)],
  address = df2017[seq(from = 4, to = length(df2017), by = 7)],
  no.killed = df2017[seq(from = 5, to = length(df2017), by = 7)],
  no.injured = df2017[seq(from = 6, to = length(df2017), by = 7)]
)

# 2018 Data ====================================================================
df2018 <- lapply(paste0("https://www.gunviolencearchive.org/reports/mass-shooting?year=2018&page=", 0:14),
                 function(url){
                   url %>% read_html() %>% 
                     html_nodes("tr td") %>% 
                     html_text()
                 })
df2018 <- unlist(df2018)
df2018 <- data.frame(
  #incident_id = df2018[seq(from = 1, to = length(df2018), by = 8)],
  incident_date = df2018[seq(from = 1, to = length(df2018), by = 7)],
  state = df2018[seq(from = 2, to = length(df2018), by = 7)],
  city_county = df2018[seq(from = 3, to = length(df2018), by = 7)],
  address = df2018[seq(from = 4, to = length(df2018), by = 7)],
  no.killed = df2018[seq(from = 5, to = length(df2018), by = 7)],
  no.injured = df2018[seq(from = 6, to = length(df2018), by = 7)]
)

# 2019 Data ====================================================================
df2019 <- lapply(paste0("https://www.gunviolencearchive.org/reports/mass-shooting?year=2019&page=", 0:11),
                 function(url){
                   url %>% read_html() %>% 
                     html_nodes("tr td") %>% 
                     html_text()
                 })
df2019 <- unlist(df2019)
df2019 <- data.frame(
  #incident_id = df2019[seq(from = 1, to = length(df2019), by = 8)],
  incident_date = df2019[seq(from = 1, to = length(df2019), by = 7)],
  state = df2019[seq(from = 2, to = length(df2019), by = 7)],
  city_county = df2019[seq(from = 3, to = length(df2019), by = 7)],
  address = df2019[seq(from = 4, to = length(df2019), by = 7)],
  no.killed = df2019[seq(from = 5, to = length(df2019), by = 7)],
  no.injured = df2019[seq(from = 6, to = length(df2019), by = 7)]
)

# Combined GV Dataset ==========================================================

# Place all year data frames in a list
all_years <- list(df2013, df2014, df2015, df2016, df2017, df2018, df2019)

# Apply the rbind command to all elements of the list using do.call()
total_gv_df <- do.call("rbind", all_years)

# The current format of the date is an integer so conver it to a character
# since this is a non-typical date setup
total_gv_df$incident_date <- as.character(total_gv_df$incident_date)

# Remove commas, split the string into separate Year, Day, Month columns,
# then recombine into one column called "new_incident_date" via paste0
# and define as a Date data type. Then change State names to abbreviations.
total_gv_df$incident_date <- str_remove(string = total_gv_df$incident_date, pattern = ",")
total_gv_df$month <- lapply(strsplit(as.character(total_gv_df$incident_date), split = " "),"[[",1)
total_gv_df$day <- lapply(strsplit(as.character(total_gv_df$incident_date), split = " "),"[[",2)
total_gv_df$year <- lapply(strsplit(as.character(total_gv_df$incident_date), split = " "),"[[",3)
total_gv_df$date <- paste0(total_gv_df$year, "-", total_gv_df$month, "-", total_gv_df$day)
total_gv_df$date <- as.Date(total_gv_df$date, "%Y-%B-%d")
total_gv_df$state <- state.abb[match(total_gv_df$state,state.name)] 
total_gv_df$city_county <- as.character(total_gv_df$city_county)
total_gv_df$state <- as.character(total_gv_df$state)
total_gv_df$no.killed <- as.integer(total_gv_df$no.killed)
total_gv_df$no.injured <- as.integer(total_gv_df$no.injured)
# Remove everything with and inside of parentheses to preserve consistency when 
# joining with the MST database.
total_gv_df$city_county <- gsub("\\s*\\([^\\)]+\\)","",as.character(total_gv_df$city_county)) 
         
hchart(mpg, "scatter", hcaes(x = displ, y = hwy, group = class))

GVA_Month <- total_gv_df %>%
  dplyr::select(date, no.killed) %>%
  group_by(Date = floor_date(x = date, unit = "month")) %>%
  tally(name = "Number of Deaths") %>%
  hchart("column", hcaes(x = Date, y = `Number of Deaths`), name = "n = ", colorByPoint = F, color = "white") %>% 
  hc_chart(zoomType = "xy") %>% 
  # hc_tooltip(crosshairs = TRUE, shared = FALSE, borderWidth = 1) %>% 
  hc_add_theme(hc_theme_db()) %>% 
  hc_title(text = "GVA Deaths In Mass Shootings - Monthly", align = "left") %>% 
  hc_exporting(enabled = TRUE, filename = "Monthly_GV_Plot") %>% 
  hc_yAxis(title = list(text = "Number of Deaths (n)", style = list(fontSize = "16px", color = "white")),
           labels = list(style = list(fontSize = "12px", color = "white"))) %>% 
  hc_xAxis(title = list(text = "Date", style = list(fontSize = "16px", color = "white")),
           labels = list(style = list(fontSize = "12px", color = "white"))) %>% 
  hc_credits(enabled = TRUE, text = "Source: https://www.gunviolencearchive.org/")
                   
# GVA_Month <- total_gv_df %>%
#   dplyr::select(date, no.killed) %>%
#   group_by(Date = floor_date(x = date, unit = "month")) %>%
#   tally(name = "Number of Deaths") %>%
#   ggplot(aes(x = Date, y = `Number of Deaths`)) + geom_col(aes(fill = `Number of Deaths`)) +
#   scale_fill_gradient(low = "blue", high = "red") +
#   theme_minimal() +
#   ggtitle("GVA Deaths In Mass Shootings - Monthly") + 
#   xlab("Date") + ylab("Number of Deaths (n)") +
#   labs(caption = "Source: https://www.gunviolencearchive.org/")

GVA_Year <- total_gv_df %>%
  dplyr::select(date, no.killed) %>%
  group_by(Date = floor_date(x = date, unit = "year")) %>%
  tally(name = "Number of Deaths") %>%
  hchart("column", hcaes(x = Date, y = `Number of Deaths`), name = "n = ", colorByPoint = F, color = "white") %>% 
  hc_chart(zoomType = "xy") %>% 
  # hc_tooltip(crosshairs = TRUE, table = TRUE, borderWidth = 1) %>%
  hc_add_theme(hc_theme_db()) %>% 
  hc_title(text = "GVA Deaths In Mass Shootings - Yearly", align = "left") %>% 
  hc_exporting(enabled = TRUE, filename = "Yearly_GV_Plot") %>% 
  hc_yAxis(title = list(text = "Number of Deaths (n)", style = list(fontSize = "16px", color = "white")),
           labels = list(style = list(fontSize = "12px", color = "white"))) %>% 
  hc_xAxis(title = list(text = "Date", style = list(fontSize = "16px", color = "white")),
           labels = list(style = list(fontSize = "12px", color = "white"))) %>% 
  hc_credits(enabled = TRUE, text = "Source: https://www.gunviolencearchive.org/")

# GVA_Year <- total_gv_df %>%
#   dplyr::select(date, no.killed) %>%
#   group_by(Date = floor_date(x = date, unit = "year")) %>%
#   tally(name = "Number of Deaths") %>%
#   ggplot(aes(x = Date, y = `Number of Deaths`)) + geom_col(aes(fill = `Number of Deaths`)) +
#   scale_fill_gradient(low = "blue", high = "red") +
#   theme_minimal() +
#   ggtitle("GVA Deaths in Mass Shootings - Yearly") + 
#   xlab("Date") + ylab("Number of Deaths (n)") +
#   labs(caption = "Source: https://www.gunviolencearchive.org/")


# Mass Shooting Tracker Data ====================================================
## NOTE AS OF 10_29_2019 MST lost the domain name and is not functioning
# dfmass <- lapply(paste0("https://www.massshootingtracker.org/data/all"),
#                  function(url){
#                    url %>% read_html() %>% 
#                      html_nodes("tr td") %>% 
#                      html_text()
#                  })
# 
# dfmass <- unlist(dfmass)
# dfmass <- data.frame(
#   incident_id = dfmass[seq(from = 1, to = length(dfmass), by = 7)],
#   incident_date = dfmass[seq(from = 2, to = length(dfmass), by = 7)],
#   Location = dfmass[seq(from = 3, to = length(dfmass), by = 7)],
#   no.killed = dfmass[seq(from = 4, to = length(dfmass), by = 7)],
#   no.injured = dfmass[seq(from = 5, to = length(dfmass), by = 7)],
#   shooters = dfmass[seq(from = 6, to = length(dfmass), by = 7)]
# )
# 
# dfmass[1] <- NULL
# 
# dfmass$city_county <- lapply(strsplit(as.character(dfmass$Location), split = ","),"[[",1)
# dfmass$state <- lapply(strsplit(as.character(dfmass$Location), split = ","),"[[",2)
# dfmass$date <- mdy(as.character(dfmass$incident_date))
# dfmass$city_county <- as.character(dfmass$city_county)
# dfmass$state <- as.character(dfmass$state)
# dfmass$no.injured <- as.integer(dfmass$no.injured)
# dfmass$no.killed <- as.integer(dfmass$no.killed)
# # Remove everything with and inside of parentheses to preserve consistency when 
# # joining with the GVA database.
# dfmass$city_county <- gsub("\\s*\\([^\\)]+\\)","",as.character(dfmass$city_county)) 

dfmass <- read.csv("dfmass.csv", fileEncoding = "latin1")
dfmass$date <- mdy(dfmass$date)

MassST_Year <- dfmass %>% 
  dplyr::select(date, no.killed) %>% 
  group_by(Date = floor_date(x = date, unit = "year")) %>% 
  tally(name = "Number of Deaths") %>% 
  ggplot(aes(x = Date, y = `Number of Deaths`)) + geom_col(aes(fill = `Number of Deaths`)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  ggtitle("Number of Gun Related Deaths in Mass Shootings Over Time") +
  labs(caption = "Source: https://www.massshootingtracker.org/")


# Combined GVA and MST Datasets ================================================

combined_gv_df <- left_join(dfmass, total_gv_df,
                          by = c("date", "city_county", "state", "no.injured", 
                                 "no.killed"))
combined_gv_df <- data.frame(combined_gv_df$date, combined_gv_df$city_county, 
                           combined_gv_df$state, combined_gv_df$no.injured, 
                           combined_gv_df$no.killed)  
colnames(combined_gv_df) <- c("date", "city_county", "state", "no.injured", 
                              "no.killed")

Combined_Year <- combined_gv_df %>% 
  dplyr::select(date, no.killed) %>% 
  group_by(Date = floor_date(x = date, unit = "year")) %>% 
  tally(name = "Number of Deaths") %>% 
  ggplot(aes(x = Date, y = `Number of Deaths`)) + geom_col(aes(fill = `Number of Deaths`)) +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal() +
  ggtitle("Number of Gun Related Deaths in Mass Shootings Over Time")

# tmap of totals  --------------------------------------------------------------

# library(tmap)         # For creating tmap
# library(tmaptools)    # For reading and processing spatial data related to tmap
# library(dplyr)        # For data wrangling
# library(sf)
# library(raster)

# us.shp <- shapefile("US_Shape_File/states.shp")

# Note there is no new hampshire or hawaii in the dataset
state_gv.df <- combined_gv_df %>% 
  group_by(state) %>% 
  tally()
state_gv.df <- as.data.frame(state_gv.df)
state_gv.df$n <- as.integer(state_gv.df$n)
state_gv.df$state <- as.character(state_gv.df$state)

state_gv.df[54,] <- c(" DC", state_gv.df$n[9] + state_gv.df$n[10]) # Combine and format D.C. datapoints

state_gv.df <- state_gv.df[c(-1),] # Remove Gardena
state_gv.df <- state_gv.df[c(-8),] # Remove Duplicate DC
state_gv.df <- state_gv.df[c(-8),] # Remove second duplicate DC
state_gv.df <- state_gv.df[c(-37),] # Remove PR
state_gv.df <- state_gv.df[c(-37),] # Remove Puerto Rico
state_gv.df <- state_gv.df[1:(nrow(state_gv.df)-1),] # Removal of DC to help leaflet

state_gv.df$state <- substr(state_gv.df$state,2,3)
names(state_gv.df) <- c("STATE_ABBR", "n")
state_gv.df$n <- as.integer(state_gv.df$n) # redefine as an integer type

# us.shp@data <-  data.frame(us.shp@data, state_gv.df[match(us.shp@data[,"STATE_ABBR"], state_gv.df[,"STATE_ABBR"]),])
# 
# GV_Total_tmap <- tm_shape(us.shp) +
#   # tm_polygons("n") +
#   tm_fill("n", title = "Total Recorded GV Incidents (n)", style = "fixed",
#           breaks = seq(0,max(state_gv.df$n),length.out = 7),
#           textNA = "No Record", 
#           colorNA = "#808080",   # <-------- color for NA values
#           palette = c("#fcf9bb", "#febb81", "#f4685c", "#bb3978", "#772181", "#2b105f", "#000005")) +
#   tm_borders() +
#   tm_layout("",
#             legend.title.size = 1,
#             legend.text.size = 0.6,
#             legend.position = c("left","bottom"),
#             legend.bg.color = "white",
#             # legend.digits = 5,
#             legend.bg.alpha = 0)


# Attempting Normalzation -----
# State population from US Census Bureau July 1st 2018

# http://web.mit.edu/11.520/www/labs/lab5/normalize.html
# https://www.census.gov/data/datasets/time-series/demo/popest/2010s-state-total.html#par_textimage_500989927 
state_pop <- read.csv("US_Pop_Census_2018.csv")


# us.shp@data <- data.frame(us.shp@data, state_pop[match(us.shp@data[,"STATE_NAME"], state_pop[,"State_Name"]),])
# 
# us.shp$Normalization <- (us.shp$n/us.shp$Population) * 100000

# The below plot shows state population normalized gun violence incidence.
# This is done by dividing the number gun violence incidents by the total
# state population multiplied by 100,000. A value of 1 indiciates for a state
# therefore indicates that for every 100,000 people in the state there will be
# one gun violence indicent.

# GV_Normal_tmap <- tm_shape(us.shp) +
#   # tm_polygons("n") +
#   tm_fill("Normalization", title = "Population Normalized GV Incidence per 100,000", style = "fixed",
#           breaks = seq(0,max(us.shp$Normalization, na.rm = TRUE),length.out = 10),
#           textNA = "No Record", 
#           colorNA = "#808080",   # <-------- color for NA values
#           palette = c("#ffffff", "#fcf2b4", "#fecd90", "#fc8a62", "#df4b67", "#b4367a", "#7e2482", "#55177d", "#190f3c", "#000003")) +
#   tm_borders() +
#   tm_layout("",
#             legend.title.size = 1,
#             legend.text.size = 0.5,
#             legend.position = c("left","bottom"),
#             legend.bg.color = "white",
#             # legend.digits = 5,
#             legend.bg.alpha = 0)

# Leaflet Interactivity --------------------------------------------------------
# library(leaflet)
# gv_palette <- colorBin("YlOrRd", domain = us.shp$Normalization, bins = 15, na.color = "#808080")
# 
# border_opacity <- as.numeric(us.shp$Normalization >= quantile(us.shp$Normalization, .75, na.rm = TRUE))
# border_opacity[is.na(border_opacity)] <- 0
# 
# labels <- sprintf(
#   "<strong>%s</strong><br/>
#   Number of GV Incidents: %g <br/>
#   GV Density: %g <br/>
#   State Population (2018, per 1,000): %g",
#   us.shp$STATE_NAME,
#   us.shp$n,
#   round(us.shp$Normalization, 2),
#   round(us.shp$Population/1000,0)
# ) %>% lapply(htmltools::HTML)
# 
# GV_Normal_leaf <- leaflet(us.shp) %>%
#   setView(lng = -100,
#           lat = 40,
#           zoom = 3) %>%
#   addPolygons(
#     data = us.shp,
#     fillColor = ~gv_palette(us.shp@data$Normalization),
#     weight = 1,  # border thickness
#     opacity = 1, # border opacity
#     color = "white", # border color
#     fillOpacity = 1,
#     label = labels) %>% 
#   addPolygons(
#     fillOpacity = 0,
#     color = "red",
#     opacity = border_opacity,
#     weight = 2,
#     label = labels,
#     labelOptions = labelOptions(
#       style = list("font-weight" = "normal", padding = "3px 8px"),
#       textsize = "15px",
#       direction = "auto")) %>% 
#   addLegend(position = 'topleft', 
#             pal =  colorBin("YlOrRd", domain = us.shp$Normalization, bins = 15, na.color = "#808080"),
#             values = ~Normalization,
#             # labels = c('0%',"","","","","","",'100%'), 
#             opacity = 0.6,      
#             title = "Relative<br>Magnitude")

# Highcharter Map --------------------------------------------------------------
mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))

GV_Count_high <- hcmap("countries/us/us-all", data = state_gv.df, value = "n",
      joinBy = c("hc-a2", "STATE_ABBR"), name = "Gun Violence Incidence",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = "")) %>% 
  hc_chart(zoomType = "xy") %>% 
  hc_chart(backgroundColor = "white") %>% 
  hc_legend(enabled = F) %>% 
  # hc_add_theme(hc_theme_db()) %>% 
  hc_title(text = "GVA Deaths In Mass Shootings: 2013 - 2019", align = "left", color = "white") %>% 
  hc_exporting(enabled = TRUE, filename = "GV_Map") %>% 
  hc_credits(enabled = TRUE, text = "Source: https://www.gunviolencearchive.org/, https://www.massshootingtracker.org/") %>% 
  hc_colorAxis(minColor = "white", maxColor = "firebrick", type = "logarithmic")


state_gv_norm.df <- merge.data.frame(x = state_gv.df, y = state_pop, by = "STATE_ABBR")
state_gv_norm.df$Normalization <- (state_gv_norm.df$n/state_gv_norm.df$Population) * 100000


GV_Norm_high <- hcmap("countries/us/us-all", data = state_gv_norm.df, value = "Normalization",
                       joinBy = c("hc-a2", "STATE_ABBR"), name = "Gun Violence Incidence",
                       dataLabels = list(enabled = TRUE, format = '{point.name}'),
                       borderColor = "#FAFAFA", borderWidth = 0.1,
                       tooltip = list(valueDecimals = 2, valuePrefix = "", valueSuffix = "")) %>% 
  hc_chart(zoomType = "xy") %>% 
  hc_chart(backgroundColor = "white") %>% 
  hc_legend(enabled = F) %>% 
  # hc_add_theme(hc_theme_db()) %>% 
  hc_title(text = "GVA Deaths In Mass Shootings: 2013 - 2019", align = "left", color = "white") %>% 
  hc_subtitle(text = "Population Normalized", align = "left", color = "white") %>% 
  hc_exporting(enabled = TRUE, filename = "GV_Map") %>% 
  hc_credits(enabled = TRUE, text = "Source: https://www.gunviolencearchive.org/, https://www.massshootingtracker.org/") %>% 
  hc_colorAxis(minColor = "white", maxColor = "firebrick", type = "logarithmic")
