library(shiny)
library(leaflet)
library(readr)
library(dplyr)

bus_stop <- read_csv(
  "https://data.ny.gov/resource/wgnh-hpq9.csv?$limit=4000"
  ) |>
  rename(lat = latitude, lng = longitude) |>
  filter(!is.na(lat) & !is.na(lng)) |>
  select(lat, lng)

# Define UI
ui <- fluidPage(
  titlePanel("NYC Bus Stops"),
  leafletOutput("nyc_map")
)

# Define server
server <- function(input, output, session) {
  output$nyc_map <- renderLeaflet({
    leaflet() |>
      addTiles() |>  # Add default OpenStreetMap tiles
      setView(lng = -74.0060, lat = 40.7128, zoom = 12) |>  # NYC
      addMarkers(data = data.frame(lng = c(-74.01344621681184), lat = c(40.70557265045331)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)