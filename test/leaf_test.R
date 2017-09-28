source("R/arrivals_presences.R")
library(leaflet)
library(rgdal)
library(data.table)


aggregate_movements <- fread("data/agg_ope_line_20xx.csv")
provinces <- readOGR("shapes/Prov2016_rprj.shp", encoding = "UTF-8")
sardinian_provinces <- subset(provinces, provinces$PROVINCIA %in% c("Sassari", "Nuoro", "Cagliari", "Oristano", "Olbia-Tempio", "Ogliastra",
                                                                    "Medio Campidano", "Carbonia-Iglesias"))


measure_selected = get_arrivals(aggregate_movements)
sardinian_provinces$measure <- sapply(sardinian_provinces$SIGLA, function(x) measure_selected[[2]][measure_selected[[1]] == x])

pal <- colorNumeric("Purples", 
                    domain = sardinian_provinces$measure)
legend_title = "Numero arrivi"

m <- leaflet() %>%
  setView(lng=8.981, lat=40.072, zoom=8) %>%
  addTiles() %>%
  addPolygons(data=sardinian_provinces, layerId = sardinian_provinces$COD_PRO, color = "#444444", weight = 1, smoothFactor = 0.5, opacity = 1.0, fillOpacity = 0.8, 
              fillColor = ~pal(measure),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE), label = sardinian_provinces$PROVINCIA, labelOptions = labelOptions(clickable = FALSE, noHide = TRUE)) %>%
  
  addLegend("bottomright", pal = pal, values = sardinian_provinces$measure, title = legend_title, opacity = 1)
m        
