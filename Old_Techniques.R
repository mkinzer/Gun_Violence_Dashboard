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


