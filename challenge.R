library(tidyverse)

BOM_data <- read_csv("data/BOM_data.csv")
BOM_data

BOM_stations <- read_csv("data/BOM_stations.csv")
BOM_stations

## Questions

# For each station, how many days have a minimum temperature, a maximum temperature 
# and a rainfall measurement recorded?
