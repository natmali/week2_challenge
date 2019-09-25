library(tidyverse)

BOM_data <- read_csv("data/BOM_data.csv")
BOM_data

BOM_stations <- read_csv("data/BOM_stations.csv")
BOM_stations

## Questions

# For each station, how many days have a minimum temperature, a maximum temperature 
# and a rainfall measurement recorded?

BOM_data %>% 
  separate(Temp_min_max, into = c("min", 'max')) %>% # seperating the max and min temperature
  filter(min != "", max != "", Rainfall != 0) %>%  # filtering for rows that have a min, max and rainfall value
  group_by(Station_number) %>% # grouping my station number
  summarise(n_rows = n()) # counting the number of rows in each group
