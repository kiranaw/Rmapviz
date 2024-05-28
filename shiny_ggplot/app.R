library(shiny)

map <- st_read("/home/kirana/Nextcloud/MO3viz/mapviz/maille.gpkg")
breedingsiteupdated <- read_csv("/home/kirana/Nextcloud/MO3viz/processjson/simev_chunks/2603/csv/breedingsiteupdate.csv")

breedingsiteupdated <- breedingsiteupdated %>%
  mutate(day = (hour / 24) + 1) %>%
  select(day, wbs, wfbs, cell)

sim_result <- inner_join(breedingsiteupdated, map,  by = c("cell" = "id_maille"))

createMap_for_a_day <- function(day){
  result_plot <- ggplot(day, aes(fill=wbs)) +
    geom_sf(color="white", size=.1) +
    theme_void() +
    scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="water in breeding site") +
    theme(legend.position="bottom")
  return(result_plot)
}

ui <- fluidPage(
  title = "Day Map",
  plotOutput("dailymap"),
  sliderInput(
    inputId = "day_in_slider",
    label = "the day for the map",
    min = 1, 
    max = 365, 
    value = 100,animate = T
  ),
  textOutput("daytext")
)

server <- function(input, output) {
  daily_map_data <- reactive({
    only_the_day <- dplyr::filter(breedingsiteupdated, day==input$day_in_slider)
    daily_map_data <- inner_join(only_the_day, map,  by = c("cell" = "id_maille"))  %>% st_as_sf()
  })
  
  output$dailymap <- renderPlot({
    createMap_for_a_day(daily_map_data())
  })
  
  output$daytext <- reactive({input$day})
}

shinyApp(ui = ui, server = server)
