# Old to be later explored

#### NEWEST state_gv.df

library(sf)
library(leaflet)
library(raster)
# us_states <- readOGR(dsn="cb_2016_us_state_500k.kml")   
us_states <- st_read(dsn = "cb_2016_us_state_500k.kml")
us_states <- st_zm(us_states, drop = T, what = "ZM")

popup1 <- paste0("<span style='color: #7f0000'><strong>US State Values</strong></span>",
                 "<br><span style='color: salmon;'><strong>State: </strong></span>", 
                 state_gv.df$STATE_ABBR, 
                 "<br><span style='color: salmon;'><strong>relative amount: </strong></span>", 
                 state_gv.df$n
)


palette <- colorBin(c('#fee0d2',  
                      '#fcbba1',
                      '#fc9272',
                      '#fb6a4a',
                      '#ef3b2c',
                      '#cb181d',
                      '#a50f15',
                      '#67000d'), 
                    bins = c(1,10,20,25,30,35,40,47))


mymap <- leaflet() %>% 
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  setView(lng = -100,
          lat = 40,
          zoom = 3) %>% 
  addPolygons(data = us_states, 
              fillColor = ~palette(state_gv.df$n),
              fillOpacity = 0.6,       
              color = "darkgrey",      
              weight = 1.5,            
              popup = popup1)%>%
  addLegend(position = 'topleft', 
            colors = c('#fee0d2',
                       '#fcbba1',
                       '#fc9272',
                       '#fb6a4a',
                       '#ef3b2c',
                       '#cb181d',
                       '#a50f15',
                       '#67000d'), 
            labels = c('0%',"","","","","","",'100%'), 
            opacity = 0.6,      
            title = "Relative<br>Magnitude") 
print(mymap)


# Leaflet Incorporation --------------------------------------------------------
library(leaflet)
library(maps)
mapStates <-  map("state", fill = TRUE, plot = FALSE)

leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

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

state_gv.df$state <- substr(state_gv.df$state,2,3)
names(state_gv.df) <- c("STATE_ABBR", "n")
state_gv.df$n <- as.integer(state_gv.df$n) # redefine as an integer type

states <- states(cb=T)
states %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(popup="Region: ", state_gv.df$STATE_ABBR, "<br>",
              "Value: ", state_gv.df$n, "<br>", stroke = FALSE)


# tmap of totals  --------------------------------------------------------------

# library(tmap)         # For creating tmap
# library(tmaptools)    # For reading and processing spatial data related to tmap
# library(dplyr)        # For data wrangling
# library(sf)
# library(raster)

# us.shp <- shapefile("US_Shape_File/states.shp")

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
# mapdata <- get_data_from_map(download_map_data("countries/us/us-all"))

# GV_Count_high <- hcmap("countries/us/us-all", data = state_gv.df, value = "n",
#       joinBy = c("hc-a2", "STATE_ABBR"), name = "Gun Violence Incidence",
#       dataLabels = list(enabled = TRUE, format = '{point.name}'),
#       borderColor = "#FAFAFA", borderWidth = 0.1,
#       tooltip = list(valueDecimals = 0, valuePrefix = "", valueSuffix = "")) %>% 
#   hc_chart(zoomType = "xy") %>% 
#   hc_chart(backgroundColor = "white") %>% 
#   hc_legend(enabled = F) %>% 
#   # hc_add_theme(hc_theme_db()) %>% 
#   hc_title(text = "GVA Deaths In Mass Shootings: 2013 - 2019", align = "left", color = "white") %>% 
#   hc_exporting(enabled = TRUE, filename = "GV_Map") %>% 
#   hc_credits(enabled = TRUE, text = "Source: https://www.gunviolencearchive.org/, https://www.massshootingtracker.org/") %>% 
#   hc_colorAxis(minColor = "white", maxColor = "firebrick", type = "logarithmic")
# 
# 
# # state_gv_norm.df <- merge.data.frame(x = state_gv.df, y = state_pop, by = "STATE_ABBR")
# # state_gv_norm.df$Normalization <- (state_gv_norm.df$n/state_gv_norm.df$Population) * 100000
# 
# 
# GV_Norm_high <- hcmap("countries/us/us-all", data = state_gv_norm.df, value = "Normalization",
#                        joinBy = c("hc-a2", "STATE_ABBR"), name = "Gun Violence Incidence",
#                        dataLabels = list(enabled = TRUE, format = '{point.name}'),
#                        borderColor = "#FAFAFA", borderWidth = 0.1,
#                        tooltip = list(valueDecimals = 2, valuePrefix = "", valueSuffix = "")) %>% 
#   hc_chart(zoomType = "xy") %>% 
#   hc_chart(backgroundColor = "white") %>% 
#   hc_legend(enabled = F) %>% 
#   # hc_add_theme(hc_theme_db()) %>% 
#   hc_title(text = "GVA Deaths In Mass Shootings: 2013 - 2019", align = "left", color = "white") %>% 
#   hc_subtitle(text = "Population Normalized", align = "left", color = "white") %>% 
#   hc_exporting(enabled = TRUE, filename = "GV_Map") %>% 
#   hc_credits(enabled = TRUE, text = "Source: https://www.gunviolencearchive.org/, https://www.massshootingtracker.org/") %>% 
#   hc_colorAxis(minColor = "white", maxColor = "firebrick", type = "logarithmic")



