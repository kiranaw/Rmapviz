library(sf)
library(readr)
library(dplyr)
library(ggplot2)
library(gganimate)
library(leaflet)
library(shiny)
library(dplyr)



###############
# DATA LOADING
##############

#map <- st_read("/home/kirana/Nextcloud/MO3viz/mapviz/maille.gpkg")
map <- st_read("/home/PChapron/Téléchargements/maille.gpkg")
map<- st_transform(map, 4326)
#breedingsiteupdated <- read_csv("/home/kirana/Nextcloud/MO3viz/processjson/simev_chunks/2603/csv/breedingsiteupdate.csv")
breedingsiteupdated <- read_csv("/home/PChapron/Téléchargements/breedingsiteupdate.csv")





breedingsiteupdated <- breedingsiteupdated %>%
  mutate(day = (hour / 24) + 1) %>%
  select(day, wbs, wfbs, cell)

# join with the map cells
# + turn into a big sf object with everything
sim_result <- inner_join(breedingsiteupdated, map,  by = c("cell" = "id_maille")) %>% st_as_sf()

######## one example map ############
map_100 <- sim_result %>%
  filter(day == 100) 


################ create map function#############
createMap_for_a_day <- function(map_100){
  
<<<<<<< HEAD
  result_plot <- ggplot(map_100, aes(fill=wbs)) +
    geom_sf(color="lightgrey", size=.001) +
    theme_void() +
    scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="water in breeding site") +
    theme(legend.position="bottom")
  return(result_plot)
=======
result_plot <- ggplot(map_100, aes(fill=wbs)) +
  geom_sf(color="lightgrey", size=.001) +
  theme_void() +
  scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="water in breeding site") +
  theme(legend.position="bottom")
return(result_plot)
>>>>>>> 70f4612384a478fac3d266c23c265f9ca9d384e3
}




##### exec time evaluation ##############
start_time <- Sys.time()
sim_result %>% filter(day==100) %>% createMap_for_a_day()
end_time <- Sys.time()
# execution time
end_time - start_time
<<<<<<< HEAD
=======





####################
# SHINY APP
######################

ui <- fluidPage(
  # this CSS trick was found on internet
  tags$style(type="text/css",".recalculating {opacity: 1.0;}"),
  title = "Day Map",
  plotOutput("dailymap",height=800),
  sliderInput(width = 1000,
    inputId = "day_in_slider",
    label = "the day for the map",
    min = 1,
    max = 365,
    value = 100,
    animate = animationOptions(interval = 500)
  )
)

server <- function(input, output) {
  # the map ggplot object created each time the slider is moved 
daily_map_data <- reactive({
  sim_result %>% filter(day==input$day_in_slider) %>% createMap_for_a_day()
    })

# the plot rendering routine
output$dailymap <- renderPlot({
    # this trick comes from internet newpage=F seems to speed up the display
    print(daily_map_data(), newpage=F)
  })
}

shinyApp(ui = ui, server = server)



>>>>>>> 70f4612384a478fac3d266c23c265f9ca9d384e3





####################
# SHINY APP
######################

ui <- fluidPage(
  # this CSS trick was found on internet
  tags$style(type="text/css",".recalculating {opacity: 1.0;}"),
  title = "Day Map",
  plotOutput("dailymap",height=800),
  sliderInput(width = 1000,
              inputId = "day_in_slider",
              label = "the day for the map",
              min = 1,
              max = 365,
              value = 100,
              animate = animationOptions(interval = 500)
  )
)

server <- function(input, output) {
  # the map ggplot object created each time the slider is moved 
  daily_map_data <- reactive({
    sim_result %>% filter(day==input$day_in_slider) %>% createMap_for_a_day()
  })
  
  # the plot rendering routine
  output$dailymap <- renderPlot({
    # this trick comes from internet newpage=F seems to speed up the display
    print(daily_map_data(), newpage=F)
  })
}

shinyApp(ui = ui, server = server)