
# The following data comes from OpenDataPhilly.org

# Dataset load
philly.gv <- read.csv("https://phl.carto.com/api/v2/sql?q=SELECT+*,+ST_Y(the_geom)+AS+lat,+ST_X(the_geom)+AS+lng+FROM+shootings&filename=shootings&format=csv&skipfields=cartodb_id")
philly.gv$date_ <- ymd(philly.gv$date_) # Convert date to proper format

philly.gv$lng <- round(philly.gv$lng,2) # Round longitude and latitude to 2 digits
philly.gv$lat <- round(philly.gv$lat,2)


# Create a continuous palette function
pal <- colorNumeric(
  palette = "magma",
  domain = philly.gv$n
)

# leaflet(data = philly.gv_round) %>% 
#   addProviderTiles(providers$CartoDB.Positron) %>% 
#   setView(lng = -75.16,
#           lat = 39.95,
#           zoom = 13) %>% 
#   # addPopups(lng = ~lng, lat = ~lat, popup = ~as.character(n)) %>%
#   # addMarkers(~lng, ~lat, label = ~as.character(n)) %>%
#   addCircles(lng = ~lng, lat = ~lat, weight = 1,
#              radius = ~sqrt(n)*50, 
#              popup = ~paste(sep = "<br/>", "<b> Number of Incidents:</b>", as.character(n)), 
#              fillColor = ~pal(n), stroke = NA, fillOpacity = 0.8
#   ) %>% 
#   addLegend("bottomright", pal = pal, values = ~n,
#             title = "Shooting Incidents (n)",
#             labFormat = labelFormat(prefix = ""),
#             opacity = 1
#   ) 


