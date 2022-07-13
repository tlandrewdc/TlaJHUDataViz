library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(tidyverse)
library(dplyr)

data = read_csv("Airfare_Report.csv")
locations = read_csv("GeoLocation.csv")

#select departure city

ET_departure1<-data %>%
  filter(city1 == "Chicago, IL") %>%
  rename(Departure = city1) %>%
  rename(Destination = city2) 
ET_departure2<-data %>%
  filter(city2 == "Chicago, IL") %>%
  rename(Departure = city2) %>%
  rename(Destination = city1) 
ET_departure <- rbind(ET_departure1, ET_departure2)
ET_departure


#return list of overall avg airfare
ET_total_fare_avg <- ET_departure %>%
  group_by(Destination) %>%
  summarise(Average_Fare = round(mean(fare), digits =2))
ET_total_fare_avg
datatable(ET_total_fare_avg, rownames= FALSE)

  
#categorize by quarter
ET_Q1 <- ET_departure %>%
  filter(quarter == "1") %>%
  group_by(Destination) %>%
  summarise(Q1_Average_Fare = mean(fare)) 
ET_Q1$Q1_Average_Fare <- round(ET_Q1$Q1_Average_Fare,2)

ET_Q2 <- ET_departure %>%
  filter(quarter == "2") %>%
  group_by(Destination) %>%
  summarise(Q2_Average_Fare = mean(fare)) 
ET_Q2$Q2_Average_Fare <- round(ET_Q2$Q2_Average_Fare,2)

ET_Q3 <- ET_departure %>%
  filter(quarter == "3")%>%
  group_by(Destination) %>%
  summarise(Q3_Average_Fare = mean(fare)) 
ET_Q3$Q3_Average_Fare <- round(ET_Q3$Q3_Average_Fare,2)

ET_Q4 <- ET_departure %>%
  filter(quarter == "4")%>%
  group_by(Destination) %>%
  summarise(Q4_Average_Fare = mean(fare)) 
ET_Q4$Q4_Average_Fare <- round(ET_Q4$Q4_Average_Fare,2)

ET_First_half <- full_join(ET_Q1,ET_Q2)
ET_Last_half <- full_join(ET_Q3,ET_Q4)
ET_Season_fare <- full_join(ET_First_half,ET_Last_half)

ET_Season_fare
SelectET <- ET_Season_fare %>%
  filter(Destination == "Austin, TX")
SelectET
FareET <- c(SelectET$Q1_Average_Fare,SelectET$Q2_Average_Fare, SelectET$Q3_Average_Fare, SelectET$Q4_Average_Fare)
Quarter <- c('1', '2','3','4')
df <- data.frame(Quarter,FareET)
df <- df %>%
  mutate(FareET = replace_na(FareET, mean(FareET, na.rm = TRUE)))

ET_Season_plot <- ggplot(data = df, aes(x=Quarter, y=FareET, fill=FareET)) +
  geom_bar(stat = "identity") + 
  coord_cartesian(ylim = c(min(FareET)-50, max(FareET)+10)) + 
  scale_fill_gradient(low = "#88D0C7", high = "#D08891")+
  labs(title="Average Fare Trend", y = "Airfare", fill='Average Fare')
ET_Season_plot

