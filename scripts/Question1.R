library(tidyverse) #connect to tidyverse library

data <- read_csv("data/BOM_data.csv") #read in BOM_data.csv & assign to data
data #tibble 323,895 x 7 col (station_number, Year, Month, Day,Temp_min_max, Rainfall Solar_exposure)

stations <- read_csv("data/BOM_stations.csv") #read in BOM_stations.csv & assign to stations
stations #tibble 7 rows x 21 col 

# Question 1 For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?
# You will first need to separate() a column to access both the minimum and maximum temperature data.
# Then, you can filter() the data to keep only rows that have minimum temperature, maximum temperature, and rainfall measurements.
# A group_by() followed by summarise() will then allow you to count the number of rows remaining for each station.

data_temp_sep <- separate(data,col=Temp_min_max, into=c("Temp_min", "Temp_max"), sep="/") # separated Temp_min_max into Temp_min and Temp_max and assigned to new dataframe

data_temp_sep%>%
  filter(Temp_min) # to remove rows with NA in this Col
filter(Temp_max) # to remove rows with NA in this Col
filter(Rainfall) # to remove rows with NA is this Col

