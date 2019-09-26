library(tidyverse) #connect to tidyverse library

data <- read_csv("data/BOM_data.csv") #read in BOM_data.csv & assign to data
data #tibble 323,895 x 7 col (station_number, Year, Month, Day,Temp_min_max, Rainfall Solar_exposure)

stations <- read_csv("data/BOM_stations.csv") #read in BOM_stations.csv & assign to stations
stations #tibble 7 rows x 21 col 

# Question 1 For each station, how many days have a minimum temperature, a maximum temperature and a rainfall measurement recorded?

data_temp_sep <- separate(data,col=Temp_min_max, into=c("Temp_min", "Temp_max"), sep="/") # separated Temp_min_max into Temp_min and Temp_max and assigned to new dataframe

data_temp_sep%>%
  filter(Temp_min!="-") %>% # to remove rows with - in this Col
  filter(Temp_max!="-") %>% # to remove rows with - in this Col
  #mutate(Temp_max=as.numeric(Temp_max))   
  filter(Rainfall!="-") %>% #to remove rows with - is this Col
  group_by(Station_number) %>% # group by station
  summarise(Day=n()) %>% #counts the number of days for each station
  write_csv("results/Question1_result.csv") #writes results to file called Question1_result.csv


#Question 2 Which month saw the lowest average daily temperature difference?

data_temp_sep%>%
    filter(Temp_min!="-") %>% # to remove rows with - in this Col
    filter(Temp_max!="-") %>% # to remove rows with - in this Col
    mutate(Temp_max=as.numeric(Temp_max),Temp_min=as.numeric(Temp_min))%>%#to make Temp_max and Temp_min a numeric col rather than chr
    mutate(Temp_diff=Temp_max-Temp_min) %>% #create a new col called Temp_diff byt subtracting Temp_min from Temp_max
    group_by(Month) %>% # group by month
    summarise(Avg_Temp_diff=mean(Temp_diff)) %>% #calculate average daily temperature difference for each month
    filter(Avg_Temp_diff==min(Avg_Temp_diff)) %>% # filter to get the month with the minimum value. 
    write_csv("results/Question2_result.csv") #writes results to file called Question2_result.csv


#Question 3 Which state saw the lowest average daily temperature difference?

int_tidy_stations<-stations%>%  #taking stations dataset assign to int_tidy_stations
gather(Station_number,data,-info) #intermediate tidy step to gather data into 3 x140 tibble with cols:info, Station_number, data

tidy_stations<-spread(int_tidy_stations,info,data)%>% # takes int_tidy_stations dataframe and creates 8x 20 dataframe where each stations info is in a row
  mutate(Station_number=as.numeric(Station_number)) # station number as numeric to allow join. to join by a col they must be the same data type

joined_data<-full_join(tidy_stations,data_temp_sep) # full join using station number of stations (tidy_stations) and bom data (data_temp_sep)

joined_data %>% 
  filter(Temp_min!="-") %>% # to remove rows with - in Temp_min
  filter(Temp_max!="-") %>% # to remove rows with - in Temp_max
  mutate(Temp_max=as.numeric(Temp_max),Temp_min=as.numeric(Temp_min))%>% #to make Temp_max and Temp_min a numeric col rather than chr
  mutate(Temp_diff=Temp_max-Temp_min) %>% #create a new col called Temp_diff byt subtracting Temp_min from Temp_max
  group_by(state) %>% # group by state
  summarise(Avg_Temp_diff=mean(Temp_diff)) %>% #calculate average daily temperature difference
  filter(Avg_Temp_diff==min(Avg_Temp_diff)) %>% # filter to get the state with the minimum value.
  write_csv("results/Question3_result.csv") #writes results to file called Question3_result.csv


#Question 4 Does the westmost (lowest longitude) or eastmost (highest longitude) weather station in our dataset have a 

joined_data %>%
  filter(Solar_exposure!="-")%>% #filter to remove rows with _ in Solar_exposure Col
  mutate(Solar_exposure=as.numeric(Solar_exposure))%>% #to make Solar_exposure Col a numeric rather than a string
  mutate(lon=as.numeric(lon))%>% #to make lon Col a numeric rather than a string
  group_by(lon)%>% #group by longditude
  summarise(Avg_Solar_exposure=mean(Solar_exposure))%>% #calculate average solar exposure for each longditude
  filter(lon==min(lon)|lon==max(lon))%>%# returns a dataframe with average solar exposure for the min and max lon
  #is the Avg_Solar_exposure @ min(lon) > Avg_Solar_exposure @ max (lon)
  write_csv("results/Question4_result.csv") #writes results to file called Question4_result.csv will write the result of line 65

