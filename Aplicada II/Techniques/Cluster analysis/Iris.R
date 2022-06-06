library(factoextra)
library(cluster)
library(tidyverse)

data <- iris[,-5]

d <- dist(data, method = "euclidean")

fviz_nbclust(data , kmeans , method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

km <- kmeans(data,
            centers = 3,
            iter.max = 100,
            nstart = 25)
km$centers
km$cluster
km$betweenss
km$withinss
km$size

fviz_cluster(km, data, ellipse.type = "convex") +
  labs(title = "Iris flower cluster")

clusplot(data, km$cluster, main = "Iris flower cluster", plotchar = FALSE,
         color = TRUE, lines = 0)

