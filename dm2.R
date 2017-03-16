library(arules)
library(dplyr)
library(ggplot2)

accid <- read.csv("Accidents_2015.csv")
accid$Accident_Index = NULL
summary(accid$Police_Force)

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

accid$Police_Force <- police_forceInterval(accid$Police_Force)
summary(accid$Number_of_Vehicles)

vehicleInterval <- function(x) {
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
accid$Number_of_Vehicles <- vehicleInterval(accid$Number_of_Vehicles)
summary(accid$Number_of_Casualties)
accid$Number_of_Casualties <- vehicleInterval(accid$Number_of_Casualties)

#grafico de x1st_class_road por cada Accident_Severity
X1stClassRoadperTypeAccident <- select(accid, X1st_Road_Class, Accident_Severity) %>% group_by(X1st_Road_Class)
#X1stClassRoadperTypeAccident$X1st_Road_Class <- factor(X1stClassRoadperTypeAccident$X1st_Road_Class, levels = c("Motorway","A(M)", "A", "B", "C", "Unclassified"))
#X1stClassRoadperTypeAccident$Accident_Severity <- factor(X1stClassRoadperTypeAccident$Accident_Severity, levels = c("Fatal", "Serious", "Slight"))
ggplot(X1stClassRoadperTypeAccident, aes(x = X1st_Road_Class, y = Accident_Severity)) + geom_histogram(binwidth = 6, stat = "identity") + facet_wrap(~ Accident_Severity) + ggtitle("Accident Severity by each 1st Road Class")


#table with 1st point of impact and casualty severity for each accident
vehicles <- read.csv("Vehicles_2015.csv")
casualties <- read.csv("Casualties_2015.csv")
require(sqldf)
matchIndex <- sqldf("SELECT v.Accident_Index, c.Casualty_Severity, v.X1st_Point_of_Impact
                    FROM casualties as c, vehicles as v WHERE v.Accident_Index = c.Accident_Index")
