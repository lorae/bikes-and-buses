library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)


# ---- Preprocess Data Outside Server ----

# Bike Data Loading
bike_data <- read_csv('data/bike_data_2023.csv') |> 
  janitor::clean_names() |> 
  mutate(trip_id = row_number(), .before = 1)

get_station_info <- function(.data, station_type = NULL) {
  .data |> 
    select(starts_with(paste0(station_type))) |> 
    distinct() |> 
    rename_with(~ str_remove(., paste0(station_type, "_"))) |> 
    mutate(across(everything(), as.character))
}

end_stations <- bike_data |> get_station_info("end")
start_stations <- bike_data |> get_station_info("start")

stations <- bind_rows(start_stations, end_stations) |> 
  slice_head(by = "station_id") |> 
  filter(station_id != "NULL") |> 
  mutate(station_id = as.numeric(station_id)) |> 
  st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

trips <- bike_data |> 
  select(trip_id:start_station_id, end_station_id, num_trips:avg_ride_time_sec) |> 
  filter(end_station_id != "NULL") |> 
  mutate(across(ends_with("station_id"), as.numeric))

# We consolidate trips from A -> B and B -> A into one row in the
# trips_unidirectional data frame
trips_undirected <- trips |>
  # Ensure pairs are treated as undirected by ordering station IDs
  mutate(
    station1_id = pmin(start_station_id, end_station_id),
    station2_id = pmax(start_station_id, end_station_id)
  ) |>
  # Group by undirected pairs
  group_by(station1_id, station2_id) |>
  summarize(
    trip_id = first(trip_id),  # Keep the first trip_id for reference
    num_trips = sum(num_trips),
    total_ride_time_sec = sum(total_ride_time_sec)
  ) |>
  mutate(
    ave_ride_time_sec = total_ride_time_sec / num_trips
  ) |>
  ungroup()

station_routes <- trips_undirected |> 
  left_join(stations |> select(station_id, start_geometry = geometry), by = c("station1_id" = "station_id")) |> 
  left_join(stations |> select(station_id, end_geometry = geometry), by = c("station2_id" = "station_id"))

station_lines <- trips_undirected
station_lines$geometry <- mapply(function(p1, p2) {
  st_cast(st_combine(c(p1, p2)), "LINESTRING")
}, station_routes$start_geometry, station_routes$end_geometry)

station_lines <- station_lines |> 
  st_as_sf() |> 
  filter(station1_id != station2_id)

# Number of trips by station, with attached geography
station_trip_counts <- trips |>
  select(start_station_id, end_station_id, num_trips) |>
  pivot_longer(cols = starts_with("start_station_id"):starts_with("end_station_id"),
               names_to = "station_type", values_to = "station_id") |>
  group_by(station_id) |>
  summarise(num_trips = sum(num_trips, na.rm = TRUE)) |> 
  left_join(stations, by = "station_id") |> 
  st_as_sf() 


# Subway Data
subway_stops <- read_csv("https://data.ny.gov/resource/i9wp-a4ja.csv?$limit=4000") |>
  rename(lat = entrance_latitude, lng = entrance_longitude) |>
  group_by(station_id) |>
  summarize(
    stop_name = first(stop_name),
    borough = first(borough),
    entrance_latitude = mean(lat, na.rm = TRUE),
    entrance_longitude = mean(lng, na.rm = TRUE),
    complex_id = first(complex_id),
    daytime_routes = first(daytime_routes)
  ) |> 
  st_as_sf(coords = c("entrance_longitude", "entrance_latitude"), crs = "WGS84")

# Buffer subway stations by 0.15 km for circles
subway_buffer <- subway_stops |> 
  st_buffer(units::set_units(0.15, km)) |> 
  mutate(label = paste0("<strong>", stop_name, "</strong>", "<br><b>Routes:</b> ", daytime_routes))

subway_lines <- read_csv("https://data.cityofnewyork.us/resource/s7zz-qmyz.csv") |> 
  st_as_sf(wkt = "the_geom", crs = 4326)

line_colors <- c(
  "1" = "#EE352E", "2" = "#EE352E", "3" = "#EE352E",
  "4" = "#00933C", "5" = "#00933C", "6" = "#00933C",
  "7" = "#B933AD", "A" = "#0039A6", "C" = "#0039A6", "E" = "#0039A6",
  "B" = "#FF6319", "D" = "#FF6319", "F" = "#FF6319", "M" = "#FF6319",
  "G" = "#6CBE45", "J" = "#996633", "Z" = "#996633", "L" = "#A7A9AC",
  "N" = "#FCCC0A", "Q" = "#FCCC0A", "R" = "#FCCC0A", "W" = "#FCCC0A",
  "S" = "#808183", "T" = "#00ADD0"
)

subway_lines_sf <- subway_lines |> 
  mutate(color = ifelse(rt_symbol %in% names(line_colors), line_colors[rt_symbol], "#000000"))

# ---- Compute which bike rides are within/out of buffer ----
in_buffer <- stations |>
  mutate(within_subway_buffer = st_within(geometry, subway_buffer) |> lengths() > 0) |>
  st_drop_geometry()

# Create tibble containing station lines within a buffer zone of a subway
station_lines_in_buffer <- station_lines |>
  st_drop_geometry() |>
  left_join(in_buffer |> rename(station1_in_buffer = within_subway_buffer), by = c("station1_id" = "station_id")) |>
  left_join(in_buffer |> rename(station2_in_buffer = within_subway_buffer), by = c("station2_id" = "station_id")) |>
  bind_cols(station_lines |> select(geometry)) |>
  st_as_sf() |>
  filter(station1_in_buffer & station2_in_buffer) 

top_station_lines_in_buffer <- station_lines_in_buffer |> 
  slice_max(order_by = num_trips, n = 20) |> 
  mutate(label = paste0("<b>Start:</b> ", station_name.x, "<br>",
                        "<b>End:</b> ", station_name.y, "<br>",
                        "<b>Rides:</b> ", num_trips))

# Create tibble containing random sample of all bike trips
station_lines_sample <- station_lines |>
  slice_sample(n = 6000)

# ---- Visualization Prep ----
lines_palette <- colorNumeric(
  palette = "viridis", domain = station_lines$num_trips
)
# Define a color palette from red to green based on `num_trips`
color_pal_quantile <- colorBin(palette = c("yellow", "purple"), 
                               domain = station_lines_sample$num_trips, 
                               bins = quantile(station_lines_sample$num_trips, probs = seq(0, 1, length.out = 10), na.rm = TRUE))
# ---- Define UI ----
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
      
      # Introduction Text
      div(style = "font-family: 'Arial'; font-size: 16px; color: #34495E; line-height: 1.6;",
          p("In 2023, Citi Bike users took over XXX rides. Although it’s difficult to know the exact routes these cyclists took, we represented their approximate routes on a map by connecting their start and endpoints."),
          p("The base colors on the map—yellow for the least-frequented locations, and violet for the most-frequented locations—represent the density of Citi Bike traffic in 2023.")
      ),
      
      # Purpose of the Map
      h4("Purpose of the Map", style = "font-family: 'Arial'; font-weight: bold; color: #2C3E50;"),
      div(style = "font-family: 'Arial'; font-size: 16px; color: #34495E; line-height: 1.6;",
          p("People bike for a variety of reasons—leisure, commuting, or even to connect with public transit. Our interest is in understanding where Citi Bike users may substitute biking for public transit."),
          p("We mapped New York City's subway system, encircling each stop with a 150-yard radius. Then, we investigated bike rides that took place between two subway stops.")
      ),
      
      # Key Routes Section
      h4("Key Routes", style = "font-family: 'Arial'; font-weight: bold; color: #2C3E50;"),
      div(style = "font-family: 'Arial'; font-size: 16px; color: #34495E; line-height: 1.6;",
          p("Here are some of the most popular routes between subway stops that we identified:"),
          tags$ul(
            tags$li("Route A: 110th St at Central Park and 116th St."),
            tags$li("Route B: 116th St between B,C lines and XX, YY."),
            tags$li("...")  # Add real examples here
          ),
          p("These routes indicate significant biking activity in areas with parallel subway lines. This suggests that people might be using Citi Bike to supplement gaps in subway coverage.")
      ),
      
      # Future Insights Section
      h4("Future Insights", style = "font-family: 'Arial'; font-weight: bold; color: #2C3E50;"),
      div(style = "font-family: 'Arial'; font-size: 16px; color: #34495E; line-height: 1.6;",
          p("This map doesn’t show the full picture, as it only includes subway lines. With more time, this map could include data about daily train ridership, bus delays, and other metrics."),
          p("However, even without additional data, Citi Bike traffic patterns give valuable insights into commuter behavior and highlight areas that might benefit from improved transit coverage.")
      )
    ),
    
    # Main Panel for Map
    mainPanel(
      leafletOutput("bike_subway_map", height = "80vh"),
      
      # Add a small footer message for clarification
      div(style = "text-align: center; margin-top: 10px; font-family: 'Arial'; font-size: 12px; color: #95A5A6;",
          "Sources: bike data from <XXX>, metro stops from <YYY>, subway lines from <ZZZ>."
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
                  group = "Subway Stations") |>
      
      # Bike routes sampled
      addPolylines(data = station_lines_sample,
                   weight = 10,
                   opacity = .05,
                   color = ~ color_pal_quantile(num_trips),  # Apply quantile-based colors
                   group = "All Bike Routes") |>
      addPolylines(data = station_lines_in_buffer,
                   weight = ~ scales::rescale(num_trips, to = c(0.1, 10)),
                   opacity = .55,
                   color = ~ lines_palette(station_lines_in_buffer$num_trips),
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
        radius = 10, blur = 15, max = 200, 
        minOpacity = 0.1,
        gradient = c("yellow", "orange", "red", "purple"),
        group = "Bike Route Heatmap"
      ) |>
      
      addLayersControl(
        overlayGroups = c(
          "Subway Stations",
          "Subway Lines",
          "All Bike Routes",
          "Bike route substitutes subway",
          "Top bike routes near subways",
          "Bike Route Heatmap"  # Add points to layers control
        ),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      # Start with these layers off
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


