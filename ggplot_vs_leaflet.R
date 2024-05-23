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
map_ninth <- sim_result %>%
  filter(day == 9)

map_ninth <- st_as_sf(map_ninth)

plot(map_ninth["wbs"], border= "white", lwd=0.2)

map_ninth %>%
  ggplot(aes(fill=wbs)) +
  geom_sf(color="white", size=.1) +
  theme_void() +
  scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="water in breeding site (ml)") +
  theme(legend.position="bottom")


######## animating series of map ###########
library(lubridate)
library(gganimate)

sim_result <- st_as_sf(sim_result)

p <- sim_result %>%
  arrange(day) %>%
  mutate(day=factor(day, levels=unique(day))) %>%
  ggplot(aes(fill=wbs)) +
  geom_sf(color="white", size=.1) +
  theme_void() +
  scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="water in breeding site (ml)") +
  theme(legend.position="bottom") +
  labs(title="", subtitle="", caption="", tag="{current_frame}") +
  theme(legend.position=c(.15,-.09), legend.title=element_text(hjust=.5), plot.title=element_text(size=rel(1.5), family="sans", face="bold"), plot.subtitle=element_text(color="#5e5855"), plot.caption=element_text(color="#867e7a"), plot.tag=element_text(hjust=0.5, color="#5e5855"), plot.tag.position=c(0.5, 0.16), panel.background=element_rect(fill="#aad3df", color=NA)) +
  transition_manual(day)

animate(p, width=1080, height=755, renderer=file_renderer(dir="anim_mola_img23/"))

anim_save("molawbstry.gif", p, width=1080, height=755)


