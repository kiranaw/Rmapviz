library(shiny)
library(ggplot2)
library(sf)
library(readr)
library(dplyr)

###############
# DATA LOADING
##############
map <- st_read("/home/kirana/Nextcloud/MO3viz/mapviz/maille.gpkg")
map_160 <- read_sf(dsn = "/home/kirana/Nextcloud/MO3viz/mapviz/shp_kw_160_2022.shp", layer = "shp_kw_160_2022") %>% select(-N10POP_T)

cell_district <- map %>% st_centroid %>%
  st_intersection(map_160) %>%
  st_drop_geometry()


map <- map %>% left_join(cell_district, by="id_maille") %>% select(-area.x, -area.y)

breedingsiteupdated <- read_csv("/home/kirana/Nextcloud/MO3viz/processjson/simev_chunks/2603/csv/breedingsiteupdate.csv")

breedingsiteupdated <- breedingsiteupdated %>%
  mutate(day = (hour / 24) + 1) %>%
  select(day, wbs, cell)

mosquitostockupdated <- read_csv("/home/kirana/Nextcloud/MO3viz/processjson/simev_chunks/2603/csv/mosquitostockupdate.csv")

mosquitostockupdated <- mosquitostockupdated %>%
  mutate(day = hour / 24) %>%
  select(day, cell, am)

sim_result <- inner_join(breedingsiteupdated, mosquitostockupdated, by = c("cell" = "cell", "day" = "day"))

sim_result <- inner_join(sim_result, map,  by = c("cell" = "id_maille")) #%>% st_as_sf()

sim_result_mean <- sim_result %>% ##st_drop_geometry %>%
  group_by(CODE, day) %>% #wbs is updated per day
  summarize(mean_wbs = mean(wbs),
            mean_am = mean(am))

sim_result <- inner_join(sim_result_mean, map_160,  by = c("CODE" = "CODE")) %>% st_as_sf()

################ create map function#############
wbs_createMap_for_a_day <- function(day){
  result_plot <- ggplot(day, aes(fill=mean_wbs)) +
    geom_sf(color="lightgrey", size=.001) +
    theme_void() +
    scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="water in breeding site") +
    theme(legend.position="bottom") +
    ggtitle("Water in Breeding Sites")
  return(result_plot)
}
 
am_createMap_for_a_day <- function(day){
  result_plot <- ggplot(day, aes(fill=mean_am)) +
    geom_sf(color="lightgrey", size=.001) +
    theme_void() +
    scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="adult mosquitoes") +
    theme(legend.position="bottom") +
    ggtitle("Adult Mosquitoes")
  return(result_plot)
}

####################
# SHINY APP
######################
ui <- fluidPage(
  # this CSS trick was found on internet
  tags$style(type="text/css",".recalculating {opacity: 1.0;}"),
  title = "Day Map",
  fluidRow(column(6, plotOutput("dailymap",height=300)),
           column(6, plotOutput("dailymap2",height=300))
  ),
  sliderInput(width = 1000,
              inputId = "day_in_slider",
              label = "day of year",
              min = 1,
              max = 365,
              value = 100,
              step = 2,
              animate = animationOptions(interval = 1500)
  )
)

server <- function(input, output) {
  # the map ggplot object created each time the slider is moved 
  daily_map_data1 <- reactive({
    sim_result %>% filter(day==input$day_in_slider) %>% wbs_createMap_for_a_day()
  })
  
  daily_map_data2 <- reactive({
    sim_result %>% filter(day==input$day_in_slider) %>% am_createMap_for_a_day()
  })

  # the plot rendering routine
  output$dailymap <- renderPlot({
    # this trick comes from internet newpage=F seems to speed up the display
    print(daily_map_data1(), newpage=F)
  })
  
  # the plot rendering routine
  output$dailymap2 <- renderPlot({
    # this trick comes from internet newpage=F seems to speed up the display
    print(daily_map_data2(), newpage=F)
  })
}

shinyApp(ui = ui, server = server)
