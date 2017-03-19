library(arules)
library(arulesViz)
library(gdata)
library(dplyr)

get_period <- function(x) {
  sapply(x, function(x) {
    if ((x >= 800 & x < 900) | (x >= 1700 & x < 1900)) {
      "rush hour"
    }
    else if (x >= 900 & x < 1200) {
      "morning"
    }
    else if (x >= 1200 & x < 1700) {
      "afternoon"
    }
    else {
      "night"
    }
  }
  )
}

police_forceInterval <- function(x) {
  sapply(x, function(x) {
    if (x >= 1 & x < 6) {
      "small"
    }
    else {
      if (x >= 6 & x < 45) {
        "medium"
      }
      else {
        "large"
      }
    }
  })
}

vehicle_And_Casualty_Interval <- function(x) {
  sapply(x, function(x) {
    if (x == 1) {
      "individual"
    }
    else {
      if (x > 1) {
        "multiple"
      }
      else {
        "none"
      }
    }
  })
}

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
# Colocar Time em formato numerico, sem :
data$Time <- as.numeric(sub(":", "", data$Time))
# Remover linhas com NA
data <- data[complete.cases(data),]
# Organizar horas por periodo
data <- mutate(data, Period = get_period(Time))
data$Time <- as.factor(data$Period)
# Discretizar numero de policias
data <- mutate(data, PoliceForce = police_forceInterval(Police_Force))
data$PoliceForce <- as.factor(data$PoliceForce)
# Discretizar numero de veiculos 
data <- mutate(data, Number_Vehicles = vehicle_And_Casualty_Interval(Number_of_Vehicles))
data$Period <- as.factor(data$Number_Vehicles)
# Discretizar numero de casualidades
data <- mutate(data, Number_Casualties = vehicle_And_Casualty_Interval(Number_of_Casualties))
data$Number_Casualties <- as.factor(data$Number_Casualties)

######################## ASSOCIATION RULES ##########################

#clustering
library(dbscan)
# Construir dados para apriori
data_apriori = data
cols <- c("X2nd_Road_Class", "Junction_Detail", "Junction_Control", 
          "Pedestrian_Crossing.Human_Control", "Pedestrian_Crossing.Physical_Facilities", 
          "Light_Conditions", "Road_Surface_Conditions", "Special_Conditions_at_Site", 
          "Carriageway_Hazards", "Urban_or_Rural_Area", "Period", "Number_of_Vehicles",
          "Number_of_Casualties", "Day_of_Week", "X1st_Road_Class",
          "X1st_Road_Number", "Road_Type", "Speed_limit", "PoliceForce", "Number_Vehicles",
          "Number_Casualties")
#data_apriori[cols] <- lapply(data_apriori[cols], as.factor)
data_apriori$Longitude = NULL
data_apriori$Latitude = NULL
data_apriori$Date = NULL
data_apriori$Time = NULL
data_apriori$X1st_Road_Number = NULL
data_apriori$X2nd_Road_Number = NULL
data_apriori$Day = NULL
data_apriori$Year = NULL
data_apriori$Accident_Index = NULL
data_apriori$Police_Force = NULL
data_apriori$LSOA_of_Accident_Location = NULL
data_apriori$Number_of_Casualties = NULL
data_apriori$Number_of_Vehicles = NULL
data_apriori$Day_of_Week = NULL
data_apriori$PoliceForce=NULL


data_apriori <- data.frame(sapply(data_apriori, function(x) if(is.factor(x)) { as.numeric(x) } else { x }))
data_apriori <- data.frame(sapply(data_apriori, function(x) if(is.factor(x)) { as.numeric(x) } else { x }))

data_apriori<-unique(data_apriori)
clusters <- dbscan(data_apriori, eps = 2, minPts = 10)


apply_apriori_clusters <- function(data_set, cluster_set){
  n_clusters <- c(1:max(unique(cluster_set$cluster)))
  subsets <- list()
  for(i in n_clusters){
    tt <- data_set[cluster_set$cluster==i,]
    tt[] <- lapply(tt, factor)
    ap_ <- apriori(tt, parameter=list(supp=0.6, conf=0.8, target="rules", minlen=2, maxlen=1000000), control=list(verbose=FALSE))
    ap_ <- ap_[!is.redundant(ap_, measure="confidence"),]
    ss <- head(sort(ap_, decreasing=TRUE, na.last=NA, by="lift"), 20)
    ss <- subset(ss,lift>1.2)
    subsets[[i]] <- ss
  }
  subsets
}
subsets <- apply_apriori_clusters(data_apriori, clusters)
save(subsets, file="report.RData")

get_unique_ass_rules <- function(data_set){
  ar <- array(dim = length(data_set))
  ar <- append(data_set[[1]],data_set[[2]])
  for(i in (3:length(data_set)-1)){
    ar <- append(ar,data_set[[i]])
    
  }
  unique(ar)
}
as <- get_unique_ass_rules(subsets)
as <- sort(as, decreasing = TRUE, na.last=NA, by="lift")
