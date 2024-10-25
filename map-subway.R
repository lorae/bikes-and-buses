library(leaflet)
library(readr)
library(dplyr)
library(sf)

# Load subway stops data
subway_stops <- read_csv(
  "https://data.ny.gov/resource/i9wp-a4ja.csv?$limit=4000"
) |>
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

# Define custom colors for each line
line_colors <- c(
  "1" = "#EE352E", "2" = "#EE352E", "3" = "#EE352E",
  "4" = "#00933C", "5" = "#00933C", "6" = "#00933C",
  "7" = "#B933AD",
  "A" = "#0039A6", "C" = "#0039A6", "E" = "#0039A6",
  "B" = "#FF6319", "D" = "#FF6319", "F" = "#FF6319", "M" = "#FF6319",
  "G" = "#6CBE45",
  "J" = "#996633", "Z" = "#996633",
  "L" = "#A7A9AC",
  "N" = "#FCCC0A", "Q" = "#FCCC0A", "R" = "#FCCC0A", "W" = "#FCCC0A",
  "S" = "#808183",
  "T" = "#00ADD0"
)

# Manually assign color to the color column
# TODO: Some subway lines are incorrectly displayed with the default black color 
# because multiple routes (e.g., A, B, C) share the same track. To accurately 
# represent each line, we need to create a separate row for each route (e.g., one
# for A, one for B, one for C), allowing the lines to overlap. This will also 
# enable users to filter the map by individual subway lines and display only the 
# selected route.
subway_lines_sf <- subway_lines_sf |>
  mutate(color = ifelse(name %in% names(line_colors), line_colors[name], "#000000"))

# Create leaflet map with subway stops and lines
leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  setView(lng = -74.0060, lat = 40.7128, zoom = 12) |>  # Center on NYC
  # Add subway stops as markers
  addCircleMarkers(
    data = subway_stops,
    lng = ~entrance_longitude,
    lat = ~entrance_latitude,
    radius = 5,
    popup = ~paste(stop_name, "Routes:", daytime_routes)
  ) |>
  # Add subway lines as polylines with specific colors
  addPolylines(
    data = subway_lines_sf,
    color = ~color,
    weight = 2,
    opacity = 0.7,
    popup = ~name
  )


