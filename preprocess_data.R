# preprocess_bike_data.R
library(tidyverse)
library(sf)
library(janitor)

# ---- Preprocess Data Outside Server ----

# Bike Data Loading
bike_data <- read_csv('data/bike_data_2023.csv') |> 
  clean_names() |> 
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

# Consolidate trips from A -> B and B -> A into one row
trips_undirected <- trips |>
  mutate(
    station1_id = pmin(start_station_id, end_station_id),
    station2_id = pmax(start_station_id, end_station_id)
  ) |>
  group_by(station1_id, station2_id) |>
  summarize(
    trip_id = first(trip_id),  
    num_trips = sum(num_trips),
    total_ride_time_sec = sum(total_ride_time_sec)
  ) |>
  mutate(ave_ride_time_sec = total_ride_time_sec / num_trips) |>
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

# Save all preprocessed data
save(station_trip_counts, station_lines, station_lines_in_buffer, top_station_lines_in_buffer, 
     subway_buffer, subway_lines_sf, file = "shiny-app/data/preprocessed_bike_data.RData")
