library(cluster)
library(ggplot2)
library(factoextra)
library(colorspace)
library(dendextend)
library(NbClust)
data("iris")
iris.scaled <- scale(iris[, -5])
#View(iris.scaled)
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
data <- iris.scaled
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=10 )$tot.withinss})
plot(1:k.max, wss,
     type="b", col="green",pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#Add Straight line to graph
abline(v = 3, col="red", lty =2)
fviz_nbclust(x, FUNcluster, method = c("silhouette", "wss"))