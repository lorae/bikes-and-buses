library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(readr)
library(dplyr)

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

station_routes <- trips |> 
  left_join(stations |> select(station_id, start_geometry = geometry), by = c("start_station_id" = "station_id")) |> 
  left_join(stations |> select(station_id, end_geometry = geometry), by = c("end_station_id" = "station_id"))

station_lines <- trips
station_lines$geometry <- mapply(function(p1, p2) {
  st_cast(st_combine(c(p1, p2)), "LINESTRING")
}, station_routes$start_geometry, station_routes$end_geometry)

station_lines <- station_lines |> 
  st_as_sf() |> 
  filter(start_station_id != end_station_id, num_trips > 40)

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
  mutate(color = ifelse(name %in% names(line_colors), line_colors[name], "#000000"))

# ---- Visualization Prep ----
lines_palette <- colorNumeric(
  palette = "viridis", domain = station_lines$num_trips
)

# ---- Define UI ----
ui <- fluidPage(
  titlePanel("Bike and Subway Map"),
  leafletOutput("bike_subway_map", height = "100vh")  # Set the height here
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
      addPolylines(data = station_lines,
                   weight = .25,
                   opacity = .2,
                   color = ~ lines_palette(station_lines$num_trips),
                   group = "Bike Routes") |>
      addPolylines(
        data = subway_lines_sf,
        color = ~color,
        weight = 2,
        opacity = 0.7,
        popup = ~name,
        group = "Subway Lines"
      ) |>
      addLayersControl(
        overlayGroups = c("Subway Stations", "Bike Routes", "Subway Lines"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)




