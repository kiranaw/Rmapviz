map <- st_read("/home/kirana/Nextcloud/MO3viz/mapviz/maille.gpkg")
map_160 <- read_sf(dsn = "/home/kirana/Nextcloud/MO3viz/mapviz/shp_kw_160_2022.shp", layer = "shp_kw_160_2022")

map_160 <- map_160 %>% select(-N10POP_T)

cell_district <- map %>% st_centroid %>%
                 st_intersection(map_160) %>%
                 st_drop_geometry()

  
map <- map %>% left_join(cell_district, by="id_maille")

map <- map %>% select(-area.x, -area.y)

# then you use dplyr & summarize to aggregate
# something like that :

# now join the map with WBS and AM
#letsgo

breedingsiteupdated <- read_csv("/home/kirana/Nextcloud/MO3viz/processjson/simev_chunks/2603/csv/breedingsiteupdate.csv")

# tambahin kolom "day", select kolom yang penting
breedingsiteupdated <- breedingsiteupdated %>%
  mutate(day = hour / 24) %>%
  select(day, hour, wbs, cell)

# join with the map cells
mapjoin <- inner_join(breedingsiteupdated, map,  by = c("cell" = "id_maille"))

#convert back to sf
map <- st_as_sf(map)


wbsmean <- mapjoin %>% ##st_drop_geometry %>%
  group_by(CODE, day) %>% #wbs is updated per day
  summarize(mean_wbs = mean(wbs))
  
wbsjoin <- inner_join(wbsmean, map_160,  by = c("CODE" = "CODE")) %>% st_as_sf()

plot(wbsjoin %>% filter(day == 30) %>% select(-CODE, -day))

#nah disini nanti coba lagi buat yang am ya
