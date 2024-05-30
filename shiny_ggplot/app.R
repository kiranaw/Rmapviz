library(shiny)

###############
# DATA LOADING
##############
map <- st_read("/home/kirana/Nextcloud/MO3viz/mapviz/maille.gpkg")
breedingsiteupdated <- read_csv("/home/kirana/Nextcloud/MO3viz/processjson/simev_chunks/2603/csv/breedingsiteupdate.csv")

breedingsiteupdated <- breedingsiteupdated %>%
  mutate(day = (hour / 24) + 1) %>%
  select(day, wbs, wfbs, cell)

# join with the map cells
# + turn into a big sf object with everything
sim_result <- inner_join(breedingsiteupdated, map,  by = c("cell" = "id_maille")) %>% st_as_sf()


createMap_for_a_day <- function(day){
  result_plot <- ggplot(day, aes(fill=wbs)) +
    geom_sf(color="lightgrey", size=.001) +
    theme_void() +
    scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="water in breeding site") +
    theme(legend.position="bottom")
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
              label = "the day for the map",
              min = 1,
              max = 365,
              value = 100,
              step = 5,
              animate = animationOptions(interval = 4000)
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
  
  # the plot rendering routine
  output$dailymap2 <- renderPlot({
    # this trick comes from internet newpage=F seems to speed up the display
    print(daily_map_data(), newpage=F)
  })
}

shinyApp(ui = ui, server = server)
