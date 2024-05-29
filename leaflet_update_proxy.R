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



createMap_for_a_day <- function(daymap){
  
  bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0, Inf)
  pal <- colorBin("YlOrRd", domain = daymap$wbs, bins = bins)
  
  leaflet(daymap) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_PUBLIC_TOKEN'))) %>%
    addPolygons(
      #fillColor = ~pal(wbs),
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
    addLegend(pal = pal, values = ~wbs, opacity = 0.7, title = NULL,
              position = "bottomright")
}

baseMap <- createMap_for_a_day(sim_result %>% filter(day==1))

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
    dplyr::filter(sim_result, day==input$day_in_slider)
    })
  
  
  output$map = renderLeaflet({
    baseMap
  }) 
  
  observe({
    
    leafletProxy("map", data = daily_map_data()) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(wbs),
        weight=0.1,
        color="white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 8,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE))
  })
  
  
}
bins <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0, Inf)
pal <- colorBin("YlOrRd", domain = sim_result$wbs, bins = bins)
shinyApp(ui = ui, server = server)
