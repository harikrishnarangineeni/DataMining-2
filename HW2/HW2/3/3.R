rm(list=ls())

library(ggdendro)
library(ggplot2)

## a ##

data.genes <- read.csv("C:/Users/harik/Desktop/STA/HW2/3/Ch10Ex11.csv",header=FALSE)

## b ##

#Complete Linkage
hierarchical_clustering_complete <- hclust(as.dist(1-cor(data.genes)),method="complete")
ggdendrogram(hierarchical_clustering_complete, labels = TRUE)


#Average Linkage
hierarchical_clustering_average <- hclust(as.dist(1-cor(data.genes)),method="average")
ggdendrogram(hierarchical_clustering_average, labels = TRUE)


#Single Linkage
hierarchical_clustering_single <- hclust(as.dist(1-cor(data.genes)),method="single")
ggdendrogram(hierarchical_clustering_single, labels = TRUE)


## c ##

PCA <- prcomp(t(data.genes))
summary(PCA)
head(PCA$rotation)

total_load <- apply(PCA$rotation,1,sum)
indices <- order(abs(total_load),decreasing = TRUE)
indices[1:10]

total_load[indices[1:10]]
