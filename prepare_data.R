library(tidyverse)
library(tidycensus)
library(tigris)
library(janitor)

# test this out with a random month from 2013
bike_data <- read_csv('~/bikes-and-buses/data/201306-citibike-tripdata.csv')