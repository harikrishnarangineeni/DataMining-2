rm(list = ls())

### Loading Required Libraries ###

library(ggplot2)
library(factoextra)
library(cluster)
library(NbClust)
library(corrplot)
library(fpc)
library(SnowballC)
library(lsa)
library(proxy)
library(kmed)
library(factoextra)
library(gRain)
library(Rgraphviz)
library(gRbase)
library(ggm)
library(gRim)
library(bnlearn)
library(igraph)


##################    a    ################################

parkinsons_load = read.delim("C:/Users/harik/Desktop/STA/hw5/New folder/1/parkinsons_updrs.data", header = TRUE, sep = ",")
parkinsons = parkinsons_load[-c(1,5,6)]
parkinsons

### hopkins_stat ###

result <- get_clust_tendency(parkinsons, n = nrow(parkinsons)-1, graph = FALSE)
result$hopkins_stat


### clusters Calculation ###

wss <- c()
for (i in 1:15) {
  km.out <- kmeans(parkinsons,centers = i, nstart = 20)
  wss[i] <- km.out$tot.withinss
  
}

plot(1:15,wss,type = 'b',main = 'Determining the Number of Clusters needed',xlab= 'No of Clusters',ylab = 'Within groups of Sum of Squares')

parkinsons.km <- kmeans(parkinsons,2, nstart = 20)
summary(parkinsons.km)
plot(parkinsons,col = km.out$cluster,main = "K-Means Clustering with 2 Clusters")

pr.comp = prcomp(parkinsons)
pc1 = pr.comp$x[,1]
pc2 = pr.comp$x[,2]


fviz_cluster(parkinsons.km, data =pr.comp$x, geom = "point",stand = FALSE, frame.type = "norm") +
             geom_text(aes(label=park$total_UPDRS )) +
             xlab("PC1") + ylab("PC2")

##################    b    ################################

BNdata = parkinsons_load
BNdata$age = as.numeric(BNdata$age)
BNdata$sex = as.numeric(BNdata$sex)

BNdata$subject. = NULL
BNdata_1 = BNdata

BNdata$motor_UPDRS = NULL
BNdata_1$total_UPDRS = NULL

cadBN <- hc(BNdata)
cadBN_1 <- hc(BNdata_1)

net1 <- as(amat(cadBN), "graphNEL")
net2 <- as(amat(cadBN_1), "graphNEL")

### plotting the best network ###

plot(net1,cex = 25)

plot(net2,cex = 20)