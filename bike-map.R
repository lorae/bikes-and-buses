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
