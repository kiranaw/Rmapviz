library(shiny)
library(readr)
library(dplyr)
library(sf)
library(leaflet)

map <- st_read("/home/kirana/Nextcloud/MO3viz/mapviz/maille.gpkg")
breedingsiteupdated <- read_csv("/home/kirana/Nextcloud/MO3viz/processjson/simev_chunks/2603/csv/breedingsiteupdate.csv")

breedingsiteupdated <- breedingsiteupdated %>%
  mutate(day = (hour / 24) + 1) %>%
  select(day, wbs, wfbs, cell)


# join with the map cells
sim_result <- inner_join(breedingsiteupdated, map,  by = c("cell" = "id_maille"))

createMap_for_a_day <- function(day){
  result_plot <- ggplot(day, aes(fill=wbs)) +
    geom_sf(color="white", size=.1) +
    theme_void() +
    scale_fill_distiller(palette="YlOrRd", direction=1, guide=guide_legend(label.position="bottom", title.position="left", nrow=1), name="water in breeding site") +
    theme(legend.position="bottom")
  return(result_plot)
}

createMap_for_a_day <- function(day){
  
  bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0, Inf)
  pal <- colorBin("YlOrRd", domain = day$wbs, bins = bins)
  
    leaflet(day) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_PUBLIC_TOKEN'))) %>%
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
}

ui <- fluidPage(
  titlePanel("MO3 Project"),
  
  sidebarLayout(
    
    sidebarPanel = sidebarPanel(
      
      sliderInput(
        inputId = "day_in_slider",
        label = "the day for the map",
        min = 1, 
        max = 365, 
        value = 100,animate = T
      ),
      
    ),
    
    
    
    mainPanel = mainPanel(
      
      leafletOutput(outputId = 'map')
      
    )
  )
)

server <- function(input, output) {
  daily_map_data <- reactive({
    only_the_day <- dplyr::filter(breedingsiteupdated, day==input$day_in_slider)
    daily_map_data <- inner_join(only_the_day, map,  by = c("cell" = "id_maille"))  %>% st_as_sf() %>% st_transform("+init=epsg:4326")
  })
  
  output$dailymap <- renderPlot({
    createMap_for_a_day(daily_map_data())
  })
  
  output$daytext <- reactive({input$day})
  

  
  output$map = renderLeaflet({
    
    createMap_for_a_day(daily_map_data())
    
  })
}

shinyApp(ui = ui, server = server)
