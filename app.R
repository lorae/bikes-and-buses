library(leaflet)
library(readr)
library(dplyr)

# Load bus stop data
subway_stops <- read_csv(
  "https://data.ny.gov/resource/i9wp-a4ja.csv?$limit=4000"
) |>
  rename(lat = entrance_latitude, lng = entrance_longitude) |>
  filter(!is.na(lat) & !is.na(lng)) |>
  select(lat, lng)

# Create leaflet map
leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  setView(lng = -74.0060, lat = 40.7128, zoom = 12) |>  # Center on NYC
  addMarkers(data = subway_stops)

