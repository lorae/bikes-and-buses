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

