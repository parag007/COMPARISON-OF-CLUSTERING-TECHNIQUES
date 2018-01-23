library(mlbench)
library(kknn)
data("iris")
specClust(iris[, 1:5], centers=3, nn=78, iter.max=100) 

cl   <- specClust(iris[, 1:5], centers=3, nn=78, iter.max=100) 
pcol <- as.character(iris$Type)
pairs(iris[2:6], pch = pcol, col = c("green", "red","blue","yellow")[cl$cluster])
table(iris$Type, cl$cluster)

aggregate(iris[,2:6],by=list(cl$cluster),mean)

library(stats)
km <- kmeans(Z, centers=k, nstart=30)
plot(data, col=km$cluster)


library(kernlab)

sc <- specc(data, centers=3)
plot(data, col=sc, pch=4)            # estimated classes (x)
points(data, col=obj$classes, pch=5) # true classes (<>)
