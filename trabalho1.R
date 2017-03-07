library(arules)
library(arulesViz)
library(gdata)

######################################READ CSV#########################################

data <- read.csv("Accidents_2015.csv", header=TRUE)

######################################READ THE GUIDES###################################

file_guide <-"Road-Accident-Safety-Data-Guide.xls"
read_guides <- function(file_from,sheet_name){
    data_name <-read.xls(file_from,sheet =sheet_name)
    return(data_name)
}

##Read Casulty Severity
Casualty_Severity <-read_guides(file_guide,"Casualty Severity")

## Read Ped Location
Ped_Location <- read_guides(file_guide,"Ped Location")

## Read Ped Movement
Ped_Movement <- read_guides(file_guide,"Ped Movement")

## Read Car Passenger 
Car_Passenger <- read_guides(file_guide,"Car Passenger")

## Read Bus Passenger
Bus_Passenger <- read_guides(file_guide,"Bus Passenger")

##Read Ped Road Maintenance Worker
Ped_Worker <- read_guides(file_guide,"Ped Road Maintenance Worker")

##Read Casualty Type
Casualty_Type <- read_guides(file_guide,"Casualty Type")

##Read IMD Decile
IMD_Decile <- read_guides(file_guide,"IMD Decile")

##Read Home Area Type
Home_Area <- read_guides(file_guide,"Home Area Type")

######################## CLEAN DATA ###############################
library(lubridate)

# Temos outra coluna para localizacao: LSOA_Of_Accident_Location
data$Location_Easting_OSGR = NULL 
data$Location_Northing_OSGR = NULL
# Nao e relevante:
data$Did_Police_Officer_Attend_Scene_of_Accident = NULL
data$Police_Force = NULL
data$Local_Authority_.Highway. = NULL
data$Local_Authority_.District. = NULL
# Tipos correctos:
data$Longitude <- as.double(data$Longitude)
data$Latitude <- as.double(data$Latitude)
data$Accident_Severity <- as.factor(data$Accident_Severity)
data$Weather_Conditions <- as.factor(data$Weather_Conditions)
# Separar datas por dia/mes/ano:
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
dates <- data$Date
parsedDates <- data.frame(Date = dates, Day=lubridate::day(dates), 
                          Month=lubridate::month(dates, label=TRUE), 
                          Year=lubridate::year(dates), WeekDay=lubridate::wday(dates, label=TRUE))
data <- data.frame(data, parsedDates)
data["Date.1"] <- NULL

######################## EXPLORATORY ANALYSIS #######################

library(ggplot2)

# scatterplot of accident locations, color coded by level of severity
ggplot(data, aes(x=Longitude, y=Latitude, colour=Accident_Severity)) + 
  geom_point() + 
  ggtitle('Accident locations and level of severity')

# severity of accidents by month
data_graph2 <- dplyr::select(data, c(Month, Accident_Severity))
data_graph2 <- dplyr::group_by(data_graph2, Accident_Severity)
data_graph2 <- dplyr::count(data_graph2, Month, Accident_Severity)
ggplot(data_graph2, aes(y=n, x=Accident_Severity)) + 
  geom_bar(stat="identity") + 
  ggtitle('Severity of accidents by month') + facet_wrap(~ Month)

# severity of accidents by weather conditions

data_graph3 <- dplyr::select(data, c(Weather_Conditions, Accident_Severity))
data_graph3 <- dplyr::group_by(data_graph3, Weather_Conditions, Accident_Severity)
data_graph3 <- dplyr::count(data_graph3, Weather_Conditions, Accident_Severity)
data_graph3 <- dplyr::mutate(data_graph3, fr=n/sum(n))
ggplot(data_graph3, aes(y=fr, x=Accident_Severity)) + 
  geom_bar(stat="identity") + 
  ggtitle('Severity of accidents by weather conditions') + facet_wrap(~ Weather_Conditions)

# number of casualties by accident

ggplot(data, aes(x=factor(0), y=Number_of_Casualties)) +
  geom_boxplot() +
  ggtitle('Number of casualties') +
  coord_flip()

