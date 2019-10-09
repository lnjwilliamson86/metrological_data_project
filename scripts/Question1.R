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

joined_data<-full_join(tidy_stations,data_temp_sep)%>% # full join using station number of stations (tidy_stations) and bom data (data_temp_sep)
  write_csv("data/joined_data.csv")

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

#ggplot challenge

#Question 1 For the Perth station (ID 9225), produce three scatter plots showing the relationship between the maximum temperature and each other measurement recorded (minimum temperature, rainfall and solar exposure).
perth_data<-joined_data %>%
  mutate(Station_number=as.numeric(Station_number),Temp_min=as.numeric(Temp_min),Temp_max=as.numeric(Temp_max),Rainfall=as.numeric(Rainfall),Solar_exposure=as.numeric(Solar_exposure))%>%
  filter(Station_number==9225)#using joined_data dataframe filter by station number 9225 Perth and assign to perth_data varaible

max_vs_min_plot<-ggplot(data=perth_data,
       mapping=aes(x=Temp_max,
                   y=Temp_min))+
  geom_point() #creates a scatter plot for perth_data of Temp_max on X axis and Temp_min on y axis and saves to max_Vs_min_plot variable.
ggsave("figures/Q1_max_vs_min.png",plot=max_vs_min_plot)# saves max_vs_min_plot as a png file in the figures folder


max_vs_rainfall_plot<-ggplot(data=perth_data,
       mapping=aes(x=Temp_max,
                   y=Rainfall))+
  geom_point() #creates a scatter plot for perth_data of Temp_max on X axis and rainfall on y axis. 
ggsave("figures/Q1_max_vs_rainfall.png",plot=max_vs_rainfall_plot)# saves max_vs_rainfall_plot as a png file in the figures folder

max_vs_solar_plot<-ggplot(data=perth_data,
                             mapping=aes(x=Temp_max,
                                         y=Solar_exposure))+
  geom_point() #creates a scatter plot for perth_data of Temp_max on X axis and solar exposure on y axis. 
ggsave("figures/Q1_max_vs_solar_exposure.png",plot=max_vs_solar_plot) # saves max_vs_solar_plot as a png file in the figures folder

#Question 2 Display these four measurements for the Perth station in a single scatter plot by using additional aesthetic mappings.
all_variables_plot<-ggplot(data=perth_data,
       mapping=aes(x=Temp_max,
                   y=Temp_min,
                   size=Rainfall,
                   colour=Solar_exposure))+
  geom_point() #creates a scatter plot for perth_data of Temp_max on x axis, temp_min on y axis, size of point by size and solar exposure by colour)
ggsave("figures/Q2_all_variables.png",plot=all_variables_plot) # saves all_vaiables_plot as a png file in the figures folder

#Question 3 Take the four plots you have produced in Q1 and Q2 and save them as a multi-panel figure.
library(cowplot) #loads the cowplot library

multipanel_plot<-plot_grid(max_vs_min_plot,max_vs_rainfall_plot,max_vs_solar_plot,all_variables_plot) #combines the plots max_vs_min_plot, max_vs_rainfall_plot, max_vs_solar_plot and all_variables_plot into a multipanel plot
ggsave("figures/Q3_multipanel_plot.png",plot=multipanel_plot,width=30,height=20,units="cm") #saves multipanel_plot as a png file specifying the size to ensure that the graph isn't squashed

#Question 4 Using the entire BOM dataset, calculate the average monthly rainfall for each station. Produce a lineplot to visualise this data and the state each station is in.

avg_rainfall<-joined_data%>% #take joined data and assigns all following code to avg_rainfall
  mutate(Station_number=as.numeric(Station_number),
         Rainfall=as.numeric(Rainfall))%>% # convert sation number and average rainfall to numeric 
  filter(Rainfall!='NA')%>% #filter to remove NA values from Rainfall col
  group_by(Station_number,Month,state)%>% #group data by Station Number, month and State
  summarise(avg_rainfall=mean(Rainfall)) # summarise rainfall to give average. produces a dataframe that is 4 x20

avg_rainfall_plot<-ggplot(data=avg_rainfall,
       mapping=aes(x=Month,
                   y=avg_rainfall,
                   group=Station_number,
                   colour=state))+ 
  geom_line() # produces a line plot using the avg_rainfall dataframe where x is month, y is avg_rainfall, group by station number (line for each station) and colour by state assigns to avg_rainfall_plot
ggsave("figures/Q4_avg_rainfall.png",plot=avg_rainfall_plot) #saves avg_rainfall_plot to a png file
 
avg_rainfall_plot_facet<-ggplot(data=avg_rainfall,
       mapping=aes(x=Month,
                   y=avg_rainfall,
                   group=Station_number,
                   colour=state))+
  geom_line()+ # produces a line plot using the avg_rainfall dataframe where x is month, y is avg_rainfall, group by station number (line for each station) and colour by state assigns to avg_rainfall_plot_facet
  facet_wrap(~state) #uses the facet wrap function to give a new plot for each state
ggsave("figures/Q4_avg_rainfall_facet.png",plot=avg_rainfall_plot_facet) #saves avg_rainfall_plot_facet to a png file

  

