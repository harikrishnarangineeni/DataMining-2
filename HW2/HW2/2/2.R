rm(list=ls())

set.seed(123)
library(ggplot2)

## a ##

x <- matrix(rnorm(20*3*50,mean=0.02,sd=0.12),ncol=50)
x[1:20,2] <- 1
x[21:40,1] <- 2
x[21:40,2] <- 2
x[41:60,1] <- 1
labels <- c(rep(1,20),rep(2,20),rep(3,20))

## b ##

PCA <- prcomp(x)
plot(PCA$x[,1:2],col=1:3,pch=20)

## c ##

KMeans.Clustering <- kmeans(x, 3, nstart = 20)
table(labels, KMeans.Clustering$cluster)

## d ##

KMeans.Clustering.2 <- kmeans(x, 2, nstart = 20)
table(labels, KMeans.Clustering.2$cluster)

## e ##

KMeans.Clustering.4 <- kmeans(x, 4, nstart = 20)
table(labels, KMeans.Clustering.4$cluster)

## f ##

KMeans.PCA <- kmeans(PCA$x[,1:2],3,nstart=20)
table(labels,KMeans.PCA$cluster)

## g ##

KMeans_Scale <- kmeans(scale(x),3,nstart=20)
table(labels,KMeans_Scale$cluster)
