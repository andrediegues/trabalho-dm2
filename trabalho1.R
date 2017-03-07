library(arules)
library(arulesViz)
library(gdata)
######################################READ THE GUIDES###################################

file_guide <-"GitProjects/trabalho-dm2/Road-Accident-Safety-Data-Guide.xls"
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


