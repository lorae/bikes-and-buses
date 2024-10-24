# Clean and summarize the 2023 raw data to be used in map
library(tidyverse)
library(duckdb)

# open duckdb connection
con <- dbConnect(duckdb(), dbdir = "/Users/gh3504/duckdb")

# read all files in directory to one table

query <- "CREATE OR REPLACE TABLE bike_data AS 
          SELECT * FROM read_csv_auto('raw_data/*/*.csv', 
                                       types={'start_station_id': 'VARCHAR'});"
dbExecute(con, query)

query <- "ALTER TABLE bike_data ADD COLUMN difference_in_seconds INTEGER;"

dbExecute(con, query)

query <- "UPDATE bike_data
SET difference_in_seconds = (EXTRACT(EPOCH FROM ended_at) - EXTRACT(EPOCH FROM started_at));"
dbExecute(con, query)

df <- tbl(con, "bike_data")

out <- df %>% 
  filter(year(started_at)==2023) %>% 
  #mutate(trip_time = as.numeric(difftime(ended_at, started_at, units = "secs"))) %>%
  #head(10)
  # mutate(trip_time = 
  #          as.numeric(ended_at)-
  #          as.numeric(started_at)) %>% 
  group_by(start_station_id, 
           end_station_id,
           start_station_name,
           end_station_name,
           start_lat, start_lng,
           end_lat, end_lng,
           rideable_type) %>% 
  summarize(num_trips = n(),
            total_ride_time_sec = sum(difference_in_seconds),
            avg_ride_time_sec = mean(difference_in_seconds),
            ) %>% 
  filter(num_trips > 20)

common_trips <- collect(out)

# save the data into `data/` as a .csv
write_csv(common_trips, "data/bike_data_2023.csv")

# close connection
dbDisconnect(con)
