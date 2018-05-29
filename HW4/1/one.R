rm(list = ls())

#source("https://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
#biocLite("RBGL")

library(Rgraphviz)
library(gRbase)
library(gRain)
library(ggm)
library(gRim)
library(bnlearn)
#library(igraph)

setwd("C:/Users/harik/Desktop/STA/HW4/New folder/1")

### load the data ###

load("cad1.RData")
names(cad1)

############## a  ##########################

cad <- list(~sex, ~smoker|sex, ~suffheartf, ~inherit|smoker, ~hyperchol|suffheartf:smoker, ~cad|inherit:hyperchol)
cad_dag <- dagList(cad)

plot(cad_dag)

### Inquire about d-separation ###

dSep(as(cad_dag, "matrix"),"sex", "suffheartf",cond = NULL)
dSep(as(cad_dag, "matrix"),"smoker", "suffheartf",cond = NULL)
dSep(as(cad_dag, "matrix"),"inherit", "suffheartf",cond = NULL)
dSep(as(cad_dag, "matrix"),"smoker","cad",c("inherit","hyperchol"))
dSep(as(cad_dag, "matrix"),"sex", "hyperchol", c("smoker", "suffheartf"))
dSep(as(cad_dag, "matrix"), "sex", "inherit", c("smoker"))

### Specify the CPD tables ###

s <- xtabs(~Sex, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
sm.s <- xtabs(~Smoker+Sex, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
in.sm <- xtabs(~Inherit+Smoker , data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
cad.in.hyp <- xtabs(~CAD+Inherit+Hyperchol, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
hyp.sm.sf <- xtabs(~Hyperchol+Smoker+SuffHeartF, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])
sf <- xtabs(~SuffHeartF, data = cad1[,c("CAD","Hyperchol","Sex","Smoker","SuffHeartF","Inherit")])



############## b  ##########################

### Build the network ###
plist <- compileCPT(list(s, sm.s, in.sm, cad.in.hyp, hyp.sm.sf, sf))
grn1 <- grain(plist)
summary(grn1)


### Compile the network ###
grn1c <- compile(grn1)
summary(grn1c)

### Propagate the the network ###
grn1c <- propagate(grn1c)
summary(grn1c)

### before absorbing evidence ###

querygrain(grn1c, nodes = c("SuffHeartF", "CAD"), type = "joint")
querygrain(grn1c, nodes = c("SuffHeartF", "CAD"), type = "conditional")
querygrain(grn1c, nodes = c("SuffHeartF", "CAD"), type = "marginal")


### after absorbing evidence ###
grn1c.ev <- setFinding(grn1c, nodes = c("Sex", "Hyperchol"), states = c("Female", "yes"))
querygrain(grn1c.ev, nodes = c("SuffHeartF", "CAD"), type = "joint")
querygrain(grn1c.ev, nodes = c("SuffHeartF", "CAD"), type = "conditional")
querygrain(grn1c.ev, nodes = c("SuffHeartF", "CAD"), type = "marginal")


############## c  ##########################
### Simulating 5 new observations ###
sim.find5 <- simulate(grn1c.ev, nsim = 5)
predict(grn1c, response = c("Smoker","CAD"),newdata = sim.find5, predictors = c("Sex","SuffHeartF","Hyperchol","Inherit"),type = "class")
sim.find5


############## d  ##########################
### Simulating 500 Data points ###
sim.find500 <- simulate(grn1c.ev, nsim = 500)
yhat = predict(grn1c, response = c("Smoker","CAD"),newdata = sim.find500, predictors = c("Sex","SuffHeartF","Hyperchol","Inherit"),type = "class")
yhat
save(yhat,file="500datapoints.RData")

### misclassification rate ###
#for smoker
tab1 = table(sim.find500$Smoker, yhat$pred$Smoker)
misclassification_rate_smoker <- (1-sum(diag(tab1))/500)*100
misclassification_rate_smoker

#for CAD
tab2 = table(sim.find500$CAD, yhat$pred$CAD)
misclassification_rate_CAD <- (1-sum(diag(tab2))/500)*100
misclassification_rate_CAD
