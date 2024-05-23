library(sf)
library(readr)
library(dplyr)
library(ggplot2)
library(gganimate)

map <- st_read("/home/kirana/Nextcloud/MO3viz/mapviz/maille.gpkg")
breedingsiteupdated <- read_csv("/home/kirana/Nextcloud/MO3viz/processjson/simev_chunks/2603/csv/breedingsiteupdate.csv")

proj4string(map)

breedingsiteupdated <- breedingsiteupdated %>%
  mutate(day = (hour / 24) + 1) %>%
  select(day, wbs, wfbs, cell)

# join with the map cells
sim_result <- inner_join(breedingsiteupdated, map,  by = c("cell" = "id_maille"))


######## one example map ############
map_100 <- sim_result %>%
  filter(day == 100)

map_100 <- st_as_sf(map_100)

plot(map_100["wbs"], border= "white", lwd=0.2)

map_100 %>%
  ggplot(aes(fill=wbs)) +
  geom_sf(color="white", size=.1) +
  theme_void() +
  scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="water in breeding site") +
  theme(legend.position="bottom")



#with leaflet
library(leaflet)
leaflet(map_100) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
  addPolygons(
    weight = 2,
    opacity = 1,
    color = "white",
    fillOpacity = 0.7) 
