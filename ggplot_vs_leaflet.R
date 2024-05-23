library(sf)
library(readr)
library(dplyr)
library(ggplot2)
library(gganimate)

map <- st_read("/home/kirana/Nextcloud/MO3viz/mapviz/maille.gpkg")
breedingsiteupdated <- read_csv("/home/kirana/Nextcloud/MO3viz/processjson/simev_chunks/2603/csv/breedingsiteupdate.csv")

breedingsiteupdated <- breedingsiteupdated %>%
  mutate(day = (hour / 24) + 1) %>%
  select(day, wbs, wfbs, cell)

# join with the map cells
sim_result <- inner_join(breedingsiteupdated, map,  by = c("cell" = "id_maille"))


######## one example map ############
map_100 <- sim_result %>%
  filter(day == 100)

map_100 <- st_as_sf(map_100)

map_100 %>%
  ggplot(aes(fill=wbs)) +
  geom_sf(color="white", size=.1) +
  theme_void() +
  scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="water in breeding site") +
  theme(legend.position="bottom")



#with leaflet
library(leaflet)
# how to add crs projection so that I can plot that in leaflet?
#Projected CRS: WGS 84 / UTM zone 47N
map100n <- st_transform(map_100, "+init=epsg:4326")

bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0, Inf)
pal <- colorBin("YlOrRd", domain = map100n$wbs, bins = bins)

leaflet(map100n) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    fillColor = ~pal(wbs),
    weight = 0.1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 8,
      color = "#666",
      fillOpacity = 0.7,
      bringToFront = TRUE)) %>%
  addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
            position = "bottomright")

