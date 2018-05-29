rm(list = ls())

demographicdata = c("Sex", "MartialStatus", "Age", "Education", "Occupation", "Income", "YearsInBayArea", "DualIncome", "TotalNoInHousehold", 
                    "NumberofChildren", "HouseholderStatus", "TypeofHome", "EthinicClassification", "LanguageinHome")


size = 9409

Sex = sample(c(1,2), size,replace = T)
MartialStatus = sample(seq(1,5), size, replace = T)
Age = sample(seq(1,7), size, replace = T)
Education = sample(seq(1,6), size, replace = T)
Occupation = sample(seq(1,9), size, replace = T)
Income = sample(seq(1,9), size, replace = T)
YearsInBayArea = sample(seq(1,5), size, replace = T)
DualIncome = sample(seq(1,3), size, replace = T)
TotalNoInHousehold = sample(seq(1,9), size, replace = T)
NumberofChildren = sample(seq(1,9), size, replace = T)
HouseholderStatus = sample(seq(1,3), size, replace = T)
TypeofHome = sample(seq(1,5), size, replace = T)
EthinicClassification = sample(seq(1,8), size, replace = T) 
LanguageinHome = sample(seq(1,3), size, replace = T)


trainingsample = data.frame(Sex, MartialStatus, Age, Education, Occupation, Income, YearsInBayArea, DualIncome, TotalNoInHousehold, 
                            NumberofChildren, HouseholderStatus, TypeofHome, EthinicClassification, LanguageinHome)

names(trainingsample) = demographicdata

### training sample (class 1) ###

trainingsample$target = 1

samplereference = trainingsample

for(i in 1:ncol(samplereference)){
  
  samplereference[,i] = sample(samplereference[,i], nrow(samplereference), replace = F)
}

### reference sample (class 0) ###

samplereference$target = 0

Mixed.data = rbind(samplereference, trainingsample); rm(samplereference, trainingsample)

### changing categorical columns ###

Mixed.data$Sex = as.factor(as.character(Mixed.data$Sex))
Mixed.data$MartialStatus = as.factor(as.character(Mixed.data$MartialStatus))
Mixed.data$Occupation = as.factor(as.character(Mixed.data$Occupation))
Mixed.data$DualIncome = as.factor(as.character(Mixed.data$DualIncome))
Mixed.data$HouseholderStatus = as.factor(as.character(Mixed.data$HouseholderStatus))
Mixed.data$TypeofHome = as.factor(as.character(Mixed.data$TypeofHome))
Mixed.data$EthinicClassification = as.factor(as.character(Mixed.data$EthinicClassification))
Mixed.data$LanguageinHome = as.factor(as.character(Mixed.data$LanguageinHome))

library(rpart)
Mixed.data$target = as.factor(as.character(Mixed.data$target))
model = rpart(target~., Mixed.data)
summary(model)
predict_new = predict(model, Mixed.data[,-c(15)])
predict_new

