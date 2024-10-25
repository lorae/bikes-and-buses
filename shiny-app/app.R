library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)

# Load preprocessed data
load("data/preprocessed_bike_data.RData")

# ---- Define UI ----
ui <- fluidPage(
  titlePanel(
    div(
      "Where Do New Yorkers Ride?",
      style = "font-family: 'Arial'; font-weight: bold; font-size: 28px; color: #2C3E50; text-align: center;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #ffffff; border-radius: 10px; padding: 20px; box-shadow: 0px 0px 10px rgba(0, 0, 0, 0.1);",
      
      div(style = "font-family: 'Arial'; font-size: 16px; color: #34495E; line-height: 1.6;",
          p(HTML('In 2023, Citi Bike users took over 35 million rides <a href="https://www.nyc.gov/html/dot/downloads/pdf/bike-share-usage-data-report-q4-2023.pdf" target="_blank">[source]</a>. Although it’s difficult to know the exact routes these cyclists took, we represented their approximate routes on a map by connecting their start and endpoints.')),
          p("The base colors on the map—yellow for the least-frequented locations, and violet for the most-frequented locations—represent the density of Citi Bike traffic in 2023.")
      ),
      
      h4("Purpose of the Map", style = "font-family: 'Arial'; font-weight: bold; color: #2C3E50;"),
      div(style = "font-family: 'Arial'; font-size: 16px; color: #34495E; line-height: 1.6;",
          p("People bike for a variety of reasons—leisure, commuting, or even to connect with public transit. Our interest is in understanding where Citi Bike users may substitute biking for public transit."),
          p("We mapped New York City's subway system, encircling each stop with a 150-yard radius. Then, we investigated all bike trips that took place between two subway stops with at least 20 Citi Bike users. You can view these trips by selecting 'Bike route substitutes subway' on the map.")
      )
    ),
    
    mainPanel(
      leafletOutput("bike_subway_map", height = "80vh"),
      
      div(style = "text-align: center; margin-top: 10px; font-family: 'Arial'; font-size: 12px; color: #95A5A6;",
          "Sources: bike data ", 
          a("[link]", href = "https://citibikenyc.com/system-data", target = "_blank"), ", ",
          "metro stops ", 
          a("[link]", href = "https://data.ny.gov/Transportation/MTA-Subway-Entrances-and-Exits-2024/i9wp-a4ja/about_data", target = "_blank"), ", ",
          "subway lines ", 
          a("[link]", href = "https://data.cityofnewyork.us/dataset/DOITT_SUBWAY_LINE_04JAN2017/s7zz-qmyz/about_data", target = "_blank"), "."
      )
    )
  )
)

# ---- Define Server logic ----
server <- function(input, output, session) {
  
  # Output the map
  output$bike_subway_map <- renderLeaflet({
    leaflet() |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -73.98565657576884, lat = 40.74846602002253, zoom = 13) |>
      
      addPolygons(data = subway_buffer,
                  weight = 0.75,
                  color = "blue",
                  fillOpacity = 0.3,
                  label = ~ lapply(label, htmltools::HTML),
                  group = "Subway Stations") |>
      
      addPolylines(data = station_lines_in_buffer,
                   weight = 0.2,
                   opacity = .55,
                   color = "black",
                   group = "Bike route substitutes subway") |>
      addPolylines(data = top_station_lines_in_buffer,
                   weight = 2,
                   opacity = 1,
                   label = ~ lapply(label, htmltools::HTML),
                   highlightOptions = highlightOptions(weight = 4, color = "lightblue"),
                   color = "navy",
                   group = "Top bike routes near subways") |>
      addPolylines(
        data = subway_lines_sf,
        color = ~color,
        weight = 4, 
        opacity = 1,
        popup = ~name,
        dashArray = "10",
        group = "Subway Lines"
      ) |>
      
      addHeatmap(
        data = station_trip_counts,
        lng = ~st_coordinates(geometry)[,1],
        lat = ~st_coordinates(geometry)[,2],
        intensity = ~num_trips,
        blur = 25, max = 1000, 
        minOpacity = 0.1,
        radius = 23,
        gradient = c("#ebdb34", "#d98704", "#ba0227", "#4b0380"),
        group = "Bike Route Heatmap"
      ) |>
      
      addLayersControl(
        overlayGroups = c(
          "Subway Stations",
          "Subway Lines",
          "Bike route substitutes subway",
          "Top bike routes near subways",
          "Bike Route Heatmap"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      hideGroup(c(
        "Subway Stations",
        "Top bike routes near subways",
        "Bike route substitutes subway",
        "All Bike Routes"
      ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
