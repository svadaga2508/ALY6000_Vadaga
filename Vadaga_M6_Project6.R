#version.string R version 3.6.3 (2020-02-06)
#Name            - Vadaga, Satyanarayana
#Course title    - ALY6000, Introduction to Analytics
#Module          - 6


install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("tidyr")
install.packages("plyr")
install.packages("tidyverse")
install.packages("ggplot2")

library("FSA")
library("FSAdata")
library("magrittr")
library("dplyr")
library("plotrix")
library("tidyr")
library("plyr")
library("tidyverse")
library("ggplot2")
library(scales)

getwd()

setwd("C:/Users/MadhuSatya/OneDrive/Documents")

#Read the data from source file
source_file = read.csv("Air_Traffic_Passenger_Statistics.csv",
                             header=TRUE, sep =",")

#Check head & tail of records
head(source_file)


# Find the record count group by year
temp <- count(source_file, "Year")
temp

# Create a new data frame with 4000-5000 records
new_source <- filter(source_file, source_file$Year>2012)
temp1 <- count(new_source, "Year")
temp1


# Descriptive Statistics
str(new_source)
summary(new_source)
labels(new_source)



# Dataframe_1
df4 <- new_source %>%
  select(GEO_Summary, Year, GEO_Region, Adjusted_Passenger_Count) %>%
  group_by(GEO_Summary,Year) %>%
  dplyr::summarize(Sum_Pass_count = sum(Adjusted_Passenger_Count))

df4$Year = factor(df4$Year)

# Dataframe_2
df5 <- new_source %>%
  select(GEO_Summary, Year, GEO_Region, Adjusted_Passenger_Count) %>%
  filter(GEO_Summary =='International') %>%  
  group_by(GEO_Region,Year) %>%
  dplyr::summarize(Sum_Pass_count = sum(Adjusted_Passenger_Count))

df5$Year = factor(df5$Year)

# Dataframe_3
df6 <- new_source %>%
  select(GEO_Summary, Year, GEO_Region, Adjusted_Passenger_Count, Published_Airline) %>%
  filter(GEO_Summary =='International' , Published_Airline %in% c("United Airlines", "Air Canada ", "Lufthansa German Airlines", "British Airways", "Cathay Pacific")) %>%  
  group_by(Published_Airline,Year) %>% 
  dplyr::summarize(Sum_Pass_count = sum(Adjusted_Passenger_Count))  

# Dataframe_4
df6 <- new_source %>%
  select(GEO_Summary, Year, GEO_Region, Adjusted_Passenger_Count, Published_Airline) %>%
  filter(GEO_Summary =='International' , Published_Airline %in% c("United Airlines", "Air Canada ", "Lufthansa German Airlines", "British Airways", "Cathay Pacific")) %>%  
  group_by(Published_Airline,Year) %>% 
  dplyr::summarize(Sum_Pass_count = sum(Adjusted_Passenger_Count))  

df6$Year = factor(df6$Year)

# Dataframe_5
df7 <- new_source %>%
  select(GEO_Summary, Year, GEO_Region, Adjusted_Passenger_Count, Published_Airline) %>%
  filter(GEO_Summary =='Domestic' , Published_Airline %in% c("United Airlines", "Virgin America", "Delta Air Lines", "Southwest Airlines", "American Airlines")) %>%  
  group_by(Published_Airline,Year) %>% 
  dplyr::summarize(Sum_Pass_count = sum(Adjusted_Passenger_Count))  

df7$Year = factor(df7$Year)


par(las=1)
par(mar = c(5.1, 4.1, 4.1, 2.1))
par(cex.axis = 0.8, cex.lab =0.8)

plot1 <- ggplot(df4, aes(x=GEO_Summary, y=Sum_Pass_count, fill = Year ))+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = label_number(suffix ="M", scale = 1e-6))

plot1 + ggtitle("Count by Geography & Year") +xlab("Geo Summary") + ylab("Passenger Count")


plot2 <-  ggplot(df4, aes(x=reorder(GEO_Summary,Sum_Pass_count ) , y=Sum_Pass_count, fill = reorder(Year,Sum_Pass_count) ))+
  labs(title = "Count by Geography & Year", X = "Geo Summary", Y = "Passenger Count")+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = label_number(suffix ="M", scale = 1e-6))

plot2 <- plot2 + scale_fill_discrete(name = "Year")                    
plot2 + ggtitle("Ordered count by Geography & Year") +xlab("Geo Summary") + ylab("Passenger Count")


plot3 <- ggplot(df5, aes(x=GEO_Region, y=Sum_Pass_count, fill = Year ))+
  labs(title = "Count by Region & Year", X = "Geo Summary", Y = "Passenger Count")+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = label_number(suffix ="M", scale = 1e-6))

plot3 + ggtitle("Count by Region & Year") +xlab("Geo Region") + ylab("Passenger Count")

plot4 <- ggplot(df5, aes(x=reorder(GEO_Region, Sum_Pass_count), y=Sum_Pass_count, fill = reorder(Year,Sum_Pass_count)))+
  labs(title = "Count by Region & Year", X = "Geo Summary", Y = "Passenger Count")+
  geom_bar(stat = "identity", position = "dodge")+
  scale_y_continuous(labels = label_number(suffix ="M", scale = 1e-6))

plot4 <- plot4 + scale_fill_discrete(name = "Year") 
plot4 + ggtitle("Count by Region & Year") +xlab("Geo Region") + ylab("Passenger Count")


plot5 <- ggplot(df6, aes(x=Year, y=Sum_Pass_count, colour = Published_Airline ))+
  geom_step(stat = "identity", position = "identity")+
  scale_y_continuous(labels = label_number(suffix ="M", scale = 1e-6))

plot5 + ggtitle("Count by Top 5 Airlines & Year - International") +xlab("Year") + ylab("Passenger Count")

scale_y_continuous(labels = label_number(suffix ="M", scale = 1e-6))

plot6 <- ggplot(df7, aes(x=Year, y=Sum_Pass_count, colour = Published_Airline,Sum_Pass_count))+
  geom_point(stat = "identity", position = "identity")+
  scale_y_continuous(labels = label_number(suffix ="M", scale = 1e-6))

plot6 + ggtitle("Count by Top 5 Airlines & Year - Domestic") +xlab("Year") + ylab("Passenger Count")

scale_y_continuous(labels = label_number(suffix ="M", scale = 1e-6))
