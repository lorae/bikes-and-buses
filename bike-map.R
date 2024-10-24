# author: Sarah Johnson
# date: 2024-10-23
library(tidyverse)
library(sf)
library(leaflet)

# ---- bike data ----
# TODO: add link to the original dataset
bike_data <- read_csv('data/201306-citibike-tripdata.csv') |> 
  janitor::clean_names() |> 
  mutate(trip_id = row_number(), .before = 1)

#' separate the data to make it easier to work with, especially later on for
#' adding layers to a map.
#' 
#' bike data can be divided into three categories:
#' trip_data <- c("trip_id", "tripduration", "starttime", "stoptime", "start_station_id", "end_station_id")
#' trip_info <- c("trip_id", "bike_id", "usertype", "birth_year", "gender")
#' stations <- contains("station")
#' 
#' the `stations` tibble will be the smallest, because station information is 
#' independent from trip information. `stations` also contains the geometries
#' for the dataset. This means we can prepare a small table of station geometry
#' and join it to trip_data later using the `station_id` columns.

## ---- get stations ----
# consolidate start & end station information
get_station_info <- function(.data, station_type = NULL) {
  
  .data |> 
    select(starts_with(paste0(station_type, "_station"))) |> 
    distinct() |> 
    rename_with(~ str_remove(., paste0(station_type, "_"))) |> 
    mutate(across(everything(), as.character))
  
}

# pull the start & end stations from the bike_data
end_stations <- bike_data |> get_station_info("end")
start_stations <- bike_data |> get_station_info("start")

#' combine start & end stations into one table. I use `slice_head()` instead of 
#' `distinct()` due to lat/long variations for the same station.
#' TODO: filter out tests & service center/ create 
stations <- bind_rows(start_stations, end_stations) |> 
  slice_head(by = "station_id") |> 
  filter(station_id != "NULL") |> 
  mutate(station_id = as.numeric(station_id))

# convert the lat & long data into a geometry column. set the crs.
stations <- stations |> 
  st_as_sf(coords = c("station_longitude", "station_latitude"), crs = "WGS84")

## ---- get trip data ----
trips <- bike_data |> 
  select(trip_id:start_station_id, end_station_id) |> 
  # TODO: filter out tests & service center
  filter(end_station_id != "NULL") |> 
  mutate(across(ends_with("station_id"), as.numeric))

#' group trip data by start & end stations, to get route info
#' summarize to count the number of rides & find other descriptive info
routes <- trips |> 
  summarise(
    trips = n(),
    total_ride_time = sum(tripduration),
    average_ride_time = mean(tripduration),
    median_ride_time = median(tripduration),
    .by = c("start_station_id", "end_station_id")
  )
# TODO: create route_id 

# add the geometries back to the stations
station_routes <- routes |> 
  left_join(stations |> select(station_id, start_geometry = geometry), by = c("start_station_id" = "station_id")) |> 
  left_join(stations |> select(station_id, end_geometry = geometry), by = c("end_station_id" = "station_id"))
  
station_lines <- routes
# combine the start and end points into a linestring
station_lines$geometry <- mapply(function(p1, p2) {
  st_cast(st_combine(c(p1, p2)), "LINESTRING")
}, station_routes$start_geometry, station_routes$end_geometry)
# is there a better way to do this?

station_lines <- station_lines |> st_as_sf() |> 
  filter(start_station_id != end_station_id)

## join station & trip ----
# count the number of trips for each station by pivoting start and end locations
station_trips <- trips |> 
  select(trip_id, start_station_id, end_station_id, tripduration) |> 
  filter(start_station_id != end_station_id) |> 
  pivot_longer(cols = ends_with("station_id"),
               # names_to = "station_point", # start or end point
               # names_pattern = "(.*)_station_id", # extracts "start" or "end" 
               values_to = "station_id") |> 
  summarise(trip_count = n(), .by = c("station_id"))
  # .by = c("station_id", "station_point") # opt: add grouping for start/end

## ---- buffer stations ---- 
# create a buffer around each station
station_buffer <- stations |> st_buffer(units::set_units(.15, km))

# add station trip data to station markers
station_buffer <- station_buffer |> 
  left_join(station_trips, by = "station_id") |> 
  mutate(label = paste0("<strong>", station_name, "</strong>",
                        "<br><b>Total Trips:</b> ", trip_count))


# ---- viz prep ----
station_palette <- colorNumeric(
  palette = "viridis", domain = station_buffer$trip_count
)

lines_palette <- colorNumeric(
  palette = "viridis", domain = station_lines$trips
)

# ---- map data ---- 
leaflet() |> 
  addProviderTiles("CartoDB.Positron")|>
  addPolygons(data = station_buffer,
              weight = 0.75,
              color = ~ station_palette(station_buffer$trip_count),
              label = ~ lapply(station_buffer$label, htmltools::HTML)) |> 
  addPolylines(data = station_lines,
               weight = .25,
               color = ~ lines_palette(station_lines$trips))
  # addMarkers(data = stations,
  #            label = stations$station_name)
