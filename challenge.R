library(tidyverse)

BOM_data <- read_csv("data/BOM_data.csv")
BOM_data

BOM_stations <- read_csv("data/BOM_stations.csv")
BOM_stations

## Questions

## Question 1: 
#For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

BOM_data %>% 
  separate(Temp_min_max, into = c("min", 'max')) %>% # Seperating the max and min temperature
  filter(min != "", max != "", Rainfall != 0) %>%  # Filtering for rows that have a min, max and rainfall value
  group_by(Station_number) %>% # Grouping my station number
  summarise(n_rows = n()) # Counting the number of rows in each group


## Question 2:
# Which month saw the lowest average daily temperature difference?

BOM_data %>% # BOM_data
  separate(Temp_min_max, into = c("min", 'max')) %>% # Seperate the columns
  filter(min != "", max != "") %>%  # Filtering for rows that have a min and max temp
  mutate(temp_difference = as.numeric(max) - as.numeric(min)) %>% # Find the difference between lowest and higherst temperatures 
  group_by(Month) %>% # Group by month
  summarise(mean_temp_dif = mean(temp_difference)) %>% # Find the average of that new column
  arrange(mean_temp_dif) # Find the lowest average temperature 


## Question 3
# Which state saw the lowest average daily temperature difference?


# Gather into 3 columns - Station ID, Type of Data (info), actual recorded value itself

BOM_stations_int <- gather(BOM_stations, Station_number, value, -info)
BOM_stations_int

# Spread into a shape with one row for each station. The 'key' argument identifies the data for the column names
# and 'value' entifes the column the will provide the data for the new cells

BOM_stations_tidy <- spread(BOM_stations_int, info, value)
BOM_stations_tidy

# Finally you want to join the two sata sets together to identify the state of each weather station. Check that the two DF's 
# have a shared column to merge and that they are the same data type. 

# I have to convert station number from a character to a value
BOM_stations_tidy <- BOM_stations_tidy %>% 
  mutate(Station_number = as.numeric(Station_number))

# Joining the two data frames
BOM_combined <- left_join(BOM_stations_tidy, BOM_data)

# Using the solution from Question 2 to work out which state saw the lowest average daily temperature difference?
BOM_combined %>% # Combined data 
  separate(Temp_min_max, into = c("min", 'max')) %>% # Seperate the columns
  filter(min != "", max != "") %>% # Filtering for rows that have a min and max temp
  mutate(temp_difference = as.numeric(max) - as.numeric(min)) %>% # Find the difference between lowest and higherst temperatures 
  group_by(state) %>% # Group by state
  summarise(mean_temp_dif = mean(temp_difference)) %>% # Find the average of the new column
  arrange(mean_temp_dif) # Find the lowest average temperature 


## Question 4
# Does the westmost (lowest longitude) or eastmost (highest longitude) weather station in our dataset have a higher average solar exposure?

# For the lowest longitude 
BOM_combined %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>% # Change Solar exposure to a value not a character
  group_by(Station_number, lon) %>% # Group by station number and longitude
  summarise(mean_solar = mean(Solar_exposure, na.rm = TRUE)) %>%  # Find the mean of Solar Exposure 
  arrange(lon) # Arrange it from smallest to largest 

# For the highest longitude 
BOM_combined %>% 
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>% # Change Solar Exposure to a value not a character
  group_by(Station_number, lon) %>% # Group by station number and longitude
  summarise(mean_solar = mean(Solar_exposure, na.rm = TRUE)) %>%  # Find the mean of Solar Exposure 
  arrange(desc(lon)) # Arrange it from largest to smallest 

## Data Visualisation ##
# Question 1: For the Perth station (ID 9225), produce three scatter plots showing the relationship between the maximum temperature 
# and each other measurement recorded (minimum temperature, rainfall and solar exposure)

BOM_combined

# Min and Max Temp
plot1 <- BOM_combined %>%
  separate(Temp_min_max, into = c("max", 'min'), sep = '/') %>%
  filter(min != "", max != "") %>%
  mutate(max = as.numeric(max), min = as.numeric(min)) %>% 
  filter(Station_number == 9225) %>% 
  ggplot(aes(
    y = max,
    x = min
  )) + geom_point()

# Max Temp and rainfall 
plot2 <- BOM_combined %>%
  separate(Temp_min_max, into = c("max", 'min'), sep = '/') %>%
  filter(min != "", max != "") %>%
  mutate(max = as.numeric(max), min = as.numeric(min), Rainfall = as.numeric(Rainfall)) %>% 
  filter(Station_number == 9225) %>% 
  ggplot(aes(
    y = Rainfall,
    x = max
  )) + geom_point()

# Max Temp and solar exposure
plot3 <- BOM_combined %>%
  separate(Temp_min_max, into = c("max", 'min'), sep = '/') %>%
  filter(min != "", max != "", Solar_exposure != "-") %>%
  mutate(max = as.numeric(max), min = as.numeric(min), Solar_exposure = as.numeric(Solar_exposure)) %>% 
  filter(Station_number == 9225) %>% 
  ggplot(aes(
    y = Solar_exposure,
    x = max
  )) + geom_point()


# Question 2: Display these four measurements for the Perth station in a single scatter plot by using 
# additional aesthetic mappings.

plot4 <- BOM_combined %>%
  separate(Temp_min_max, into = c("max", 'min'), sep = '/') %>%
  filter(min != "", max != "", Solar_exposure != "-") %>%
  mutate(max = as.numeric(max), min = as.numeric(min), Solar_exposure = as.numeric(Solar_exposure), Rainfall = as.numeric(Rainfall)) %>% 
  filter(Station_number == 9225) %>% 
  ggplot(aes(
    y = min,
    x = max,
    colour = Solar_exposure, 
    size = Rainfall
  )) + geom_point()

plot4

# Question 3: Take the four plots you have produced in Q1 and Q2 and save them as a multi-panel figure

install.packages("cowplot") # New package
library(cowplot)

multipanel_BOM_combined <- plot_grid(plot1, plot2, plot3, plot4) # Creating a multi-panel plot 
multipanel_BOM_combined

ggsave("figures/multipanel_BOM_combined.png", multipanel_BOM_combined) # Saving the plot as an image in my figures folder 

# Question 4: Using the entire BOM dataset, calculate the average monthly rainfall for each station. Produce a lineplot 
# to visualise this data and the state each station is in

BOM_combined %>% 
  mutate(Rainfall = as.numeric(Rainfall)) %>% 
  filter(Rainfall > 0) %>% 
  group_by(Month, Station_number) %>% 
  mutate(amr = mean(Rainfall)) %>% 
  ggplot(aes(
    x = Month,
    y = amr,
    colour = name,
    linetype = state
  )) + geom_line()

