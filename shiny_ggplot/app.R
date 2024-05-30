library(shiny)

###############
# DATA LOADING
##############
map <- st_read("/home/kirana/Nextcloud/MO3viz/mapviz/maille.gpkg")
breedingsiteupdated <- read_csv("/home/kirana/Nextcloud/MO3viz/processjson/simev_chunks/2603/csv/breedingsiteupdate.csv")

breedingsiteupdated <- breedingsiteupdated %>%
  mutate(day = (hour / 24) + 1) %>%
  select(day, wbs, cell)

mosquitostockupdated <- read_csv("/home/kirana/Nextcloud/MO3viz/processjson/simev_chunks/2603/csv/mosquitostockupdate.csv")

mosquitostockupdated <- mosquitostockupdated %>%
  mutate(day = hour / 24) %>%
  select(day, cell, am)

sim_result <- inner_join(breedingsiteupdated, mosquitostockupdated, by = c("cell" = "cell", "day" = "day"))

sim_result <- inner_join(sim_result, map,  by = c("cell" = "id_maille")) %>% st_as_sf()

################ create map function#############
wbs_createMap_for_a_day <- function(day){
  result_plot <- ggplot(day, aes(fill=wbs)) +
    geom_sf(color="lightgrey", size=.001) +
    theme_void() +
    scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="water in breeding site") +
    theme(legend.position="bottom") +
    ggtitle("Water in Breeding Sites")
  return(result_plot)
}
 
am_createMap_for_a_day <- function(day){
  result_plot <- ggplot(day, aes(fill=am)) +
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
              step = 5,
              animate = animationOptions(interval = 4000)
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
