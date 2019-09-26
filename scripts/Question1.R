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
  filter(Temp_min!="-") %>% # to remove rows with - in this Col
  filter(Temp_max!="-") %>% # to remove rows with - in this Col
  mutate(Temp_max=as.numeric(Temp_max))   
  filter(Rainfall!="-") %>% #to remove rows with - is this Col
  group_by(Station_number) %>% # group by station
  summarise(Day=n()) %>% #counts the number of days for each station
  write_csv("results/Question1_result.csv") #writes results to file called Question1_result.csv


#Question 2 Which month saw the lowest average daily temperature difference?
#This question can be answered using just the BOM_data file.
#In addition to the functions you used above, this question will need a mutate() to calculate the temperature difference.
#The temperature values are stored as characters after you have run separate() (see the <chr> in the second row if you print the
#data frame to the console). To be able to calculate the difference without an error, you will need to convert them to numeric values with as.numeric() first.
#For rows that are missing a temperature measurement, the temperature difference will be NA. How will you deal with these in the rest of the analysis?

data_temp_sep%>%
    filter(Temp_min!="-") %>% # to remove rows with - in this Col
    filter(Temp_max!="-") %>% # to remove rows with - in this Col
    mutate(Temp_max=as.numeric(Temp_max),Temp_min=as.numeric(Temp_min))%>%#to make Temp_max and Temp_min a numeric col rather than chr
    mutate(Temp_diff=Temp_max-Temp_min) %>% #create a new col called Temp_diff byt subtracting Temp_min from Temp_max
    group_by(Month) %>% # group by month
    summarise(Avg_Temp_diff=mean(Temp_diff)) %>% #calculate average daily temperature difference
   #summarise to show min and the month
    arrange(Avg_Temp_diff) %>% #arranges Avg_Temp_diff in ascending order 
    write_csv("results/Question2_result.csv") #writes results to file called Question2_result.csv


#Question 3 Which state saw the lowest average daily temperature difference?
#State information is found in the BOM_stations file. So we will need to join this with our previous dataset.
#The station data is not in a tidy format however, as each station is recorded in it’s own column. 
#(Why is this data not tidy?)
#To tidy it before merging, you will need to gather() the station data into an intermediate form that has three columns, 
#one for the station ID number, one for the type of data being recorded (the info column in the original data), 
#and one for the actual recorded value itself. (Is this intermediate data tidy?)
#This data frame can then be spread() into a shape with one row for each station. Remember that the key argument to spread() 
#identifies the column that will provide the data for the new column names, and the value argument identifies the column that 
#will provide the data for the new cells.
#Finally, you will want to join the two datasets together to identify the state of each weather station. If you run into errors at this step, 
#check that the two data frames have a shared column to merge, and that they are the same data type (eg. you can’t merge a character column with a numeric column).

int_tidy_stations<-stations%>%  #taking stations dataset assign to tidy_stations
gather(Station_number,data,-info) #intermediate tidy step to gather data into 3 x140 tibble with cols:info, Station_number, data

tidy_stations<-spread(int_tidy_stations,info,data)%>% # takes int_tidy_stations dataframe and creates 8x 20 dataframe where each stations info is in a row
  mutate(Station_number=as.numeric(Station_number)) # station number as numeric to allow join

joined_data<-full_join(tidy_stations,data_temp_sep) %>% #
  















#Question 4 Does the westmost (lowest longitude) or eastmost (highest longitude) weather station in our dataset have a higher average solar exposure?
#This question will need both the BOM_data and the BOM_stations file.
#You will not need any new verbs other than what you have used in previous answers.
#If answering this is final question is easy, spend some time reviewing your entire script to see if there are any ways you can improve it. Are there any repeated steps that you could save as an intermediate variable? Could you add some comments to make your code understandable?

  
#Optional extension
#Design your own question. What is a question you had after exploring the contents of the data? Or was there something that surprised you when working with the data.


#Sharing your work
#After answering these questions, swap scripts with another pair, along with any instructions they will need to make sure it works. Can you get their script to run?
#Compare your code for the first four questions. Are there any major differences in how you went about solving them?