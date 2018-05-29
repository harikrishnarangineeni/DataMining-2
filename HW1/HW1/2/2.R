rm(list = ls())

### import libraries ###

library(MASS)
data("Boston")
library(ggplot2)

### Visualize the data using histograms ###

for(i in 1:ncol(Boston))
{
  if(typeof(Boston[,i]) != "integer"){
    print(ggplot(data = Boston, aes(Boston[,i])) + geom_histogram() + xlab(names(Boston)[i]))
  }
}

summary(Boston)

### First five rows ###

head(Boston)
Boston[1:5,]


table(sapply(Boston$crim, function(x) ifelse(x < 0.08, 1, 0)))

#### ordered variables ####

### 1 - low, 0 - high ###
Boston$newcrim = sapply(Boston$crim, function(x) ifelse(x < 0.08, 1, 0))
Boston$newindus = sapply(Boston$indus, function(x) ifelse(x > 10, 0, 1))
Boston$newnox = sapply(Boston$nox, function(x) ifelse(x >= 0.7, 0, 1))
Boston$newage = sapply(Boston$age, function(x) ifelse(x > 75, 0, 1))
Boston$newdis = sapply(Boston$dis, function(x) ifelse(x <= 3.7, 1, 0))
Boston$newtax = sapply(Boston$tax, function(x) ifelse(x >= 500, 0, 1))
Boston$newptratio = sapply(Boston$ptratio, function(x) ifelse(x >= 19, 0, 1))
Boston$newblack = sapply(Boston$black, function(x) ifelse(x >= 396, 0, 1))
Boston$newmedv = sapply(Boston$medv, function(x) ifelse(x>= 30, 0, 1))


##### Convert to a binary incidence matrix #####

subset = Boston[,c(15:23)]
subset = data.frame(lapply(subset, as.factor))
transaction_subset = as(subset, "transactions")
itemFrequencyPlot(transaction_subset)

#### Apply the Apriori Algorithm ####

library(arules)
rules = apriori(transaction_subset, parameter = list(support = 0.05))
summary(rules)



lowcrime = subset(rules, subset = rhs %in% "newcrim=1")
lowdistance = subset(rules, subset = rhs %in% "newdis=1")
lowpupilratio = subset(rules, subset = rhs %in% "newptratio=1")

inspect(head(sort(lowcrime, by = "lift")))
inspect(head(sort(lowdistance, by = "lift")))
inspect(head(sort(lowpupilratio, by = "lift")))

### regression model ###

subset = data.frame(lapply(subset, function(x) as.numeric(as.character(x))))
model = lm(newptratio~., subset)
summary(model)
