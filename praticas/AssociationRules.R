#Load  the  dataset Groceries
library(arules)
library(arulesViz)
data(Groceries)

# use class to inspect the type of data set
class(Groceries)

#summary to get more information on the data set.
summary(Groceries)

#size on the data set
size(Groceries)

#inspect to see the first five transactions.
inspect(Groceries[1:5])
#or
inspect(head(Groceries,n=5L))

#duplicated transactions (unique -Removes The Duplicated Elements from the DataSet)
library(dplyr)
duplicated(Groceries)
sum(duplicated(Groceries)==TRUE) # number os duplicated items

#itemFrequency to see the relative frequency of each item
itemFrequency(Groceries)

#itemFrequencyPlot, plot the top 5 more frequent items
Groceries %>% itemFrequency() %>% sort(decreasing=TRUE) %>% head(n=5L) %>% barplot() 
#itemFrequencyPlot() dosen´t work here, why???? needs to be barplot          
itemFrequencyPlot(Groceries,topN=5)


#plot  the  items  that  have  a  support  value  of  at least 0.1
itemFrequencyPlot(Groceries,support=0.1)

#obtain  the  frequent  itemsets  for  a minimum support of 0.01
#class of the object returned
#How many frequent itemsets were found (A:98)
itemsets <- apriori(Groceries,parameter=list(supp=0.01, conf=0.8))
itemsets

# 5 most frequent itemsets. What’s their size?
top5 <- Groceries %>% itemFrequency() %>% sort(decreasing=TRUE) %>% head(n=5L)
top5size <- top5 * length(Groceries)
top5size

# subset of closed frequent itemsets, and  substet of maximal frequent itemsets 
closedFrequent <- apriori(Groceries,parameter=list(supp=0.01, conf=0.8,target="closed frequent itemsets"))
maximalFrequent <- apriori(Groceries,parameter=list(supp=0.01, conf=0.8,target="maximally frequent itemsets"))

#generate association rules (????)
rules <- apriori(Groceries, parameter = list(supp=0.01, conf=0.5, target="rules"))
summary(rules)

#association rules with minsup=0.01 and minconf=0.25
rules2 <- apriori(Groceries, parameter = list(supp=0.01,conf=0.25, target="rules"))
plot(rules2)
summary(rules2)
inspect(rules2)
quality(rules2)

#rules with a lift value above 2
rules.lift <- subset(rules2, lift>2)

#select  the  rules  that  have  lift  value  above  2  and  the  items  “whole milk” or “yogurt” on the consequent.
#Inspect the selected rules by decreasing order of their lift value
#???
sort(inspect(rules.lift["rhs"=="whole milk" || "rhs"=="yogurt"]),decreasing = TRUE,by="lift")