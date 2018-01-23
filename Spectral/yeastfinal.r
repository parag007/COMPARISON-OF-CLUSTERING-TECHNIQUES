library(mlbench)
library(kknn)
data("yeast")
specClust(yeast[, 1:17], centers=10, nn=90, iter.max=100) 

cl   <- specClust(yeast[, 1:17], centers=10, nn=90, iter.max=100) 
pcol <- as.character(yeast$Type)
pairs(yeast[2:18], pch = pcol, col = c("green", "red","blue","yellow")[cl$cluster])
table(yeast$Type, cl$cluster)

aggregate(yeast[,2:18],by=list(cl$cluster),mean)

library(stats)
km <- kmeans(Z, centers=k, nstart=100)
plot(data, col=km$cluster)


library(kernlab)

sc <- specc(data, centers=10)
plot(data, col=sc, pch=4)            # estimated classes (x)
points(data, col=obj$classes, pch=5) # true classes (<>)
