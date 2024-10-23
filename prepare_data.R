library(tidyverse)
library(tidycensus)
library(tigris)
library(janitor)
library(sf)
library(stringr)

# Read in citi bike data
# test this out with a random month from 2013
# bike_data <- read_csv('~/bikes-and-buses/data/201306-citibike-tripdata.csv')
bike_data <- read_csv('data/201306-citibike-tripdata.csv')

# Spatial join NYC blocks with Citi Bike data in order to get census block
# that each ride originates in

# get proper map projection
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

blocks <- blocks("NY", year = 2020) %>%
  st_transform(., wgs84) %>%
  clean_names() %>%
  mutate(
    countyfp10 = as.numeric(paste0(statefp20, countyfp20)),
    `Census Year` = 2020,
    `Census Block Group` = as.numeric(str_sub(blockce20, 1, 1)),
    `Full FIPS (tract)` = ifelse(
      nchar(geoid20) == 14,
      as.numeric(str_sub(geoid20, 1, 10)),
      ifelse(nchar(geoid20) == 15, as.numeric(str_sub(geoid20, 1, 11)), NA)
    )
  ) %>%
  select(
    geometry,
    `Census Year`,
    `State FIPS` = statefp20,
    `County FIPS` = countyfp20,
    `Census Tract Code` = tractce20,
    `Census Block Code` = blockce20,
    `Census Block Group`,
    `Full FIPS (block)` = geoid20,
    `Full FIPS (tract)`
  ) %>%
  mutate(across(where(is.character), as.numeric))

# join bike data to census geometries
bike_data_geo <- bike_data %>%
  filter(!is.na(`start station latitude`)) %>%
  st_as_sf(.,
           coords = c("start station longitude", "start station latitude"),
           crs = wgs84
  ) %>%
  st_join(., blocks) %>%
  mutate(
    Longitude = st_coordinates(.)[, 1],
    Latitude = st_coordinates(.)[, 2],
    `State FIPS` = str_pad(`State FIPS`, 2, side = "left", pad = "0"),
    `County FIPS` = str_pad(`County FIPS`, 3, side = "left", pad = "0"),
    `Census Tract Code` = str_pad(`Census Tract Code`, 6, side = "left", pad = "0"),
    `Census Block Code` = str_pad(`Census Block Code`, 4, side = "left", pad = "0"),
    `Full FIPS (block)` = str_pad(`Full FIPS (block)`, 15, side = "left", pad = "0"),
    `Full FIPS (tract)` = str_pad(`Full FIPS (tract)`, 11, side = "left", pad = "0")
  ) %>%
  rename(GEOID.Block = `Full FIPS (block)`) %>% 
   st_drop_geometry() # convert to data.frame (instead of spatial structure)

bike_data_geo %>% 
  # We can just use the GEOID.Block, which was assigned based on the start station geography
  count(GEOID.Block) -> num_rides_per_block

# Now that we are able to get the # of trips started in each block group, we need to prepare data
# for mapping
# We can't just use the existing data, beacuse we want to plot all blocks in NYC
wgs84 <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

nyc_blocks <- blocks(state = "NY") %>% 
  filter(COUNTYFP20 %in% c("005", "047", "081", "085", "061")) %>% 
  left_join(num_rides_per_block, by = join_by("GEOID20"=="GEOID.Block")) %>% 
  st_transform(., crs = wgs84) 
# define palette

pal <- colorNumeric(
  palette = "BuPu",
  domain = nyc_blocks$n
)

labels <- sprintf(
  "<strong>%s</strong><br/> Num Rides within Block:  %g",
  nyc_blocks$GEOID20,
  nyc_blocks$n
) %>%
  map(htmltools::HTML)

# move this later
leaflet() |>
  # addTiles() |>  # Add default OpenStreetMap tiles
  addProviderTiles("CartoDB.Positron")|>
  addPolygons(data = nyc_blocks,
              weight = 0.3, 
              fillColor = ~adjustcolor(pal(n), alpha.f = 0.7), 
              color = "gray",
              label = labels) %>% 
  setView(lng = -74.0060, lat = 40.7128, zoom = 12) |>  # NYC
  addMarkers(data = data.frame(lng = c(-74.01344621681184), lat = c(40.70557265045331)))


