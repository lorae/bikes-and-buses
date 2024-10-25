library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)

# Load preprocessed data
load("data/preprocessed_bike_data.RData")

# ---- Define UI ----
ui <- fluidPage(
  titlePanel(
    tagList(
      div(
        "Where Do New Yorkers Ride?",
        style = "font-family: 'Arial'; font-weight: bold; font-size: 28px; color: #2C3E50; text-align: center;"
      ),
      div(
        "By Grace Hartley, Sarah Johnson, and Lorae Stojanovic",
        style = "font-family: 'Arial'; font-size: 16px; color: #34495E; text-align: center; margin-top: 5px;"
      )
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
      ),
      
      h4("What We Found", style = "font-family: 'Arial'; font-weight: bold; color: #2C3E50;"),
      div(style = "font-family: 'Arial'; font-size: 16px; color: #34495E; line-height: 1.6;",
          p("Here are some of the most popular routes between subway stops that we identified:"),
          tags$ul(
            tags$li("111 St: Lexington Ave. to Lenox Ave."),
            tags$li("117 St: Frederick Douglass Blvd. to Lenox Ave."),
            tags$li("5th Ave & 59th St to Central Park West & W 72nd St")  # Add real examples here
          ),
          p("These routes indicate significant biking activity in areas with parallel subway lines. This suggests that people might be using Citi Bike to supplement gaps in subway coverage.")
      ),
      
      h4("Next Steps", style = "font-family: 'Arial'; font-weight: bold; color: #2C3E50;"),
      div(style = "font-family: 'Arial'; font-size: 16px; color: #34495E; line-height: 1.6;",
          p("This map doesn’t show the full picture, as it only includes subway lines. With more time, this map could include data about daily train ridership, bus delays, and other metrics."),
          p("However, even without additional data, Citi Bike traffic patterns give valuable insights into commuter behavior and highlight areas that might benefit from improved transit coverage.")
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
      
      # Subway buffer and bike routes
      addPolygons(data = subway_buffer,
                  weight = 0.75,
                  color = "blue",
                  fillOpacity = 0.3,
                  label = ~ lapply(label, htmltools::HTML),
                  group = "Subway stations") |>
      
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
        group = "Subway lines"
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
        group = "Bike route heatmap"
      ) |>
      
      addLayersControl(
        overlayGroups = c(
          "Subway stations",
          "Subway lines",
          "Bike route substitutes subway",
          "Top bike routes near subways",
          "Bike route heatmap"
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      hideGroup(c(
        "Subway stations",
        "Top bike routes near subways",
        "Bike route substitutes subway",
        "All bike routes"
      ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
