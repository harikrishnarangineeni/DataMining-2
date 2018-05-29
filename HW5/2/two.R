rm(list = ls())

### Loading Required Libraries ###

library(magrittr)
library(dplyr)
library(arules)
library(gRain)
library(Rgraphviz)
library(gRbase)
library(ggm)

train = read.csv("C:/Users/harik/Desktop/STA/hw5/New folder/2/train.csv", header = TRUE)
test = read.csv("C:/Users/harik/Desktop/STA/hw5/New folder/2/test.csv", header = TRUE)
gender = read.csv("C:/Users/harik/Desktop/STA/hw5/New folder/2/gender.csv", header = TRUE)


test <- cbind(test, gender)
test <- test[,-12]
titanic_data <- rbind(train,test)
rm(test)
rm(train)

titanic_data <- na.omit(titanic_data)
titanic_data <- as.data.frame(titanic_data)

titanic_data[["Age"]] = ordered(cut(titanic_data[["Age"]], c(0,13,59,80)), labels = c("child", "adult","senior"))
titanic_data[["Survived"]] = factor(titanic_data$Survived, labels = c("yes","no"))
titanic_data[["Pclass"]] = as.factor(titanic_data$Pclass)
titanic_data$Parch <- as.factor(titanic_data$Parch)
titanic_data[["SibSp"]] <- as.factor(titanic_data$SibSp)



titanic_data[["PassengerId"]] = NULL
titanic_data[["Ticket"]] = NULL
titanic_data[["Name"]] = NULL
titanic_data[["Cabin"]] = NULL
titanic_data[["Embarked"]] = NULL
titanic_data[["Fare"]] = NULL
titanic_data = titanic_data[,-7]

titanic_data_matrix <- as(titanic_data, "transactions")

summary(titanic_data_matrix)

###  Binary Incidence Matrix  ###

x11()
itemFrequencyPlot(titanic_data_matrix, support = 0, cex.names = 0.8)
itemFrequencyPlot(titanic_data_matrix, support = 0.1, cex.names = 0.8)

rules <- apriori(titanic_data_matrix, parameter = list(support = 0.01, confidence = 0.6))
summary(rules)
rules
inspect(rules[1:10])


### Survived Passengers  ####

rules<-apriori(titanic_data_matrix, parameter=list(support=0.01,confidence=0.6))

rules_survived <- subset(rules, subset = rhs %in% "Survived=yes" & lift>1)
summary(rules_survived)
survived <- inspect(rules_survived)
survived$lhs
inspect(head(sort(rules_survived,by="confidence"),n=10))
inspect((sort(rules_survived,by="lift")))

#### Dead passengers #####

rules<-apriori(titanic_data_matrix, parameter=list(support=0.01,confidence=0.6))

rules_dead <- subset(rules, subset = rhs %in% "Survived=no" & lift>1)
summary(rules_dead)
dead <- inspect(rules_dead)

inspect(head(sort(rules_dead,by="confidence"),n=10))
inspect(head(sort(rules_dead,by="lift"),n=20))