if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

pkgs <- c("cluster",  "NbClust")
install.packages(pkgs)

library(factoextra)
library(cluster)
library(NbClust)

# Load the data
data("Glass")
head(Glass)

Glass.scaled <- scale(Glass[c('RI','Na','Mg','Al','Si','K','Ca','Ba','Fe')])
# Remove species column (5) and scale the data
#Glass.scaled <- scale(Glass[, ])

# K-means clustering
set.seed(123)
km.res <- kmeans(Glass.scaled, 9, nstart = 25)

# k-means group number of each observation
km.res$cluster

# Visualize k-means clusters
fviz_cluster(km.res, data = Glass.scaled, geom = "point",
             stand = FALSE, frame.type = "norm")

#elbow method
set.seed(123)
# Compute and plot wss for k = 2 to k = 15
k.max <- 15 # Maximal number of clusters
data <- Glass.scaled
#wss <- sapply(1:k.max, 
              #function(k){kmeans(data, k, nstart=10 )$tot.withinss})

#plot(1:k.max, wss,
     #type="b", pch = 19, frame = FALSE,
     #xlab="Number of clusters K",
     #ylab="Total within-clusters sum of squares")
#abline(v = 3, lty =2)

mydata <- data
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
abline(v = 6, lty =2)

#fviz_nbclust(Glass.scaled, kmeans, method = "wss") 
 # geom_vline(xintercept =6 , linetype =2 )

