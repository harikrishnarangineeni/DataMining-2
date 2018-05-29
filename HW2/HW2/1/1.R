rm(list=ls())

library(ISLR)
library(ggdendro)
library(ggplot2)
set.seed(1234)

## a ##

data_US <- (USArrests)
hierarchical_clustering <- hclust(dist(data_US), method="complete")
ggdendrogram(hierarchical_clustering, labels = TRUE)

## b ##

three_distinct_clusters <- cutree(hierarchical_clustering,3)
three_distinct_clusters

plot(three_distinct_clusters)

table(three_distinct_clusters)

## c ##

data_US_Scale <- scale(USArrests)
hierarchical_clustering_Scale <- hclust(dist(data_US_Scale),method="complete")
ggdendrogram(hierarchical_clustering_Scale, labels = TRUE)

## d ##

scaled_tree <- cutree(hierarchical_clustering_Scale,3)
plot(scaled_tree)

table(scaled_tree)
