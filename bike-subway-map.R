library(tidyverse)
library(sf)
library(leaflet)
library(readr)
library(dplyr)

# ---- Bike Data ----
# Load bike data
bike_data <- read_csv('data/bike_data_2023.csv') |> 
  janitor::clean_names() |> 
  mutate(trip_id = row_number(), .before = 1)

# Get station information (start and end stations)
get_station_info <- function(.data, station_type = NULL) {
  .data |> 
    select(starts_with(paste0(station_type))) |> 
    distinct() |> 
    rename_with(~ str_remove(., paste0(station_type, "_"))) |> 
    mutate(across(everything(), as.character))
}

end_stations <- bike_data |> get_station_info("end")
start_stations <- bike_data |> get_station_info("start")

# Combine start and end stations
stations <- bind_rows(start_stations, end_stations) |> 
  slice_head(by = "station_id") |> 
  filter(station_id != "NULL") |> 
  mutate(station_id = as.numeric(station_id))

# Convert stations to sf object
stations <- stations |> 
  st_as_sf(coords = c("lng", "lat"), crs = "WGS84")

# Prepare trip data
trips <- bike_data |> 
  select(trip_id:start_station_id, end_station_id, num_trips:avg_ride_time_sec) |> 
  filter(end_station_id != "NULL") |> 
  mutate(across(ends_with("station_id"), as.numeric))

# Add geometries to routes
station_routes <- trips |> 
  left_join(stations |> select(station_id, start_geometry = geometry), by = c("start_station_id" = "station_id")) |> 
  left_join(stations |> select(station_id, end_geometry = geometry), by = c("end_station_id" = "station_id"))

station_lines <- trips
station_lines$geometry <- mapply(function(p1, p2) {
  st_cast(st_combine(c(p1, p2)), "LINESTRING")
}, station_routes$start_geometry, station_routes$end_geometry)

depot_stations <- c(255, 3017, 3019, 3020)
station_lines <- station_lines |> 
  st_as_sf() |> 
  filter(start_station_id != end_station_id, !start_station_id %in% depot_stations, !end_station_id %in% depot_stations, num_trips > 40)

station_trips <- trips |> 
  select(num_trips, start_station_id, end_station_id, total_ride_time_sec) |> 
  filter(start_station_id != end_station_id) |> 
  pivot_longer(cols = ends_with("station_id"), values_to = "station_id") |> 
  summarise(trip_count = sum(num_trips), .by = c("station_id"))

# Create a buffer around bike stations
station_buffer <- stations |> 
  st_buffer(units::set_units(.15, km)) |> 
  left_join(station_trips, by = "station_id") |> 
  mutate(label = paste0("<strong>", station_name, "</strong>",
                        "<br><b>Total Trips:</b> ", trip_count))

# ---- Subway Data ----
# Load subway stops data
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
  )

# Load subway lines data
subway_lines <- read_csv("https://data.cityofnewyork.us/resource/s7zz-qmyz.csv")

# Convert subway lines to sf object
subway_lines_sf <- subway_lines |> 
  st_as_sf(wkt = "the_geom", crs = 4326)  # Assuming WGS84 CRS

# Define custom colors for subway lines
line_colors <- c(
  "1" = "#EE352E", "2" = "#EE352E", "3" = "#EE352E",
  "4" = "#00933C", "5" = "#00933C", "6" = "#00933C",
  "7" = "#B933AD", "A" = "#0039A6", "C" = "#0039A6", "E" = "#0039A6",
  "B" = "#FF6319", "D" = "#FF6319", "F" = "#FF6319", "M" = "#FF6319",
  "G" = "#6CBE45", "J" = "#996633", "Z" = "#996633", "L" = "#A7A9AC",
  "N" = "#FCCC0A", "Q" = "#FCCC0A", "R" = "#FCCC0A", "W" = "#FCCC0A",
  "S" = "#808183", "T" = "#00ADD0"
)

# Assign colors to subway lines
subway_lines_sf <- subway_lines_sf |> 
  mutate(color = ifelse(name %in% names(line_colors), line_colors[name], "#000000"))

# ---- Visualization Prep ----
# Bike stations color palette
station_palette <- colorNumeric(palette = "viridis", domain = station_buffer$trip_count)

# Subway line color palette (not required due to custom colors already assigned)
lines_palette <- colorNumeric(palette = "viridis", domain = station_lines$num_trips)

# ---- Create the Combined Map ----
leaflet() |> 
  addProviderTiles("CartoDB.Positron") |>
  # Add bike stations as polygons
  addPolygons(data = station_buffer,
              weight = 0.75,
              color = ~ station_palette(station_buffer$trip_count),
              label = ~ lapply(station_buffer$label, htmltools::HTML),
              group = "Bike Stations") |>
  # Add bike routes as polylines
  addPolylines(data = station_lines,
               weight = .25,
               opacity = .2,
               color = ~ lines_palette(station_lines$num_trips),
               group = "Bike Routes") |>
  # Add subway stops as markers
  addCircleMarkers(
    data = subway_stops,
    lng = ~entrance_longitude,
    lat = ~entrance_latitude,
    radius = 5,
    popup = ~paste(stop_name, "Routes:", daytime_routes),
    group = "Subway Stops"
  ) |>
  # Add subway lines as polylines with specific colors
  addPolylines(
    data = subway_lines_sf,
    color = ~color,
    weight = 2,
    opacity = 0.7,
    popup = ~name,
    group = "Subway Lines"
  ) |>
  # Add layers control
  addLayersControl(
    overlayGroups = c("Bike Stations", "Bike Routes", "Subway Stops", "Subway Lines"),
    options = layersControlOptions(collapsed = FALSE)
  )
