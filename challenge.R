library(tidyverse)

BOM_data <- read_csv("data/BOM_data.csv")
BOM_data

BOM_stations <- read_csv("data/BOM_stations.csv")
BOM_stations

## Questions

# Question 1: 
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

BOM_data %>% 
  separate(Temp_min_max, into = c("min", 'max')) %>% # seperating the max and min temperature
  filter(min != "", max != "", Rainfall != 0) %>%  # filtering for rows that have a min, max and rainfall value
  group_by(Station_number) %>% # grouping my station number
  summarise(n_rows = n()) # counting the number of rows in each group

# Question 2:
# Which month saw the lowest average daily temperature difference?

BOM_data_temp <- BOM_data %>% # BOM_data
  separate(Temp_min_max, into = c("min", 'max')) %>% # Seperate the columns
  filter(min != "", max != "") # Filtering for rows that have a min and max temp
  
BOM_data_temp 
  
BOM_data_temp_dif <- mutate(BOM_data_temp, temp_difference = as.numeric(max) - as.numeric(min)) # Find the difference between lowest and higherst temperatures 

BOM_data_temp_dif

grouped_by_month <- group_by(BOM_data_temp_dif, Month) # Group by month

grouped_by_month
  
summarised_BOM_data <- summarise(grouped_by_month, mean_temp_dif = mean(temp_difference)) # Find the average of that new column

summarised_BOM_data

arrange(summarised_BOM_data, mean_temp_dif) # Find the lowest average temperature 

# Question 3
# Which state saw the lowest average daily temperature difference?


# Gather into 3 columns - Station ID, Type of Data (info), actual recorded value itself
BOM_stations_int <- gather(BOM_stations, station_id, value, -info)
BOM_stations_int

# Spread into a shape with one row for each station. The 'key' argument identifies the data for the column names
# and 'value' entifes the column the will provide the data for the new cells

BOM_stations_tidy <- spread(BOM_stations_int, info, value)
BOM_stations_tidy

# Finally you want to join the two sata sets together to identify the state of each weather station. Check that the two DF's 
# have a shared column to merge and that they are the same data type. 
