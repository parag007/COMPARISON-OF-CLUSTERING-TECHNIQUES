library(mlbench)
library(kknn)
data("glass")
specClust(glass[, 1:9], centers=6, nn=98, iter.max=100) 

cl   <- specClust(glass[, 1:9], centers=6, nn=98, iter.max=100) 
pcol <- as.character(glass$Type)
pairs(glass[2:10], pch = pcol, col = c("green", "red","blue","yellow")[cl$cluster])
table(glass$Type, cl$cluster)

aggregate(glass[,2:10],by=list(cl$cluster),mean)

library(stats)
km <- kmeans(Z, centers=k, nstart=30)
plot(data, col=km$cluster)


library(kernlab)

sc <- specc(data, centers=6)
plot(data, col=sc, pch=4)            # estimated classes (x)
points(data, col=obj$classes, pch=5) # true classes (<>)
