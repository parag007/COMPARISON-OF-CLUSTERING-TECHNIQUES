yeast <- read.table(url("http://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.data"), header = FALSE)
names(yeast)<- c("SequenceName", "mcg", "gvh", "alm", "mit", "erl", "pox", "vac", "nuc", "LocalizationSite")


pca <- princomp(yeast[, 2:9], cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
PrincipalComponent1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
PrincipalComponent2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)
#PrincipalComponent3 <- -1*pc.comp[,3] # principal component 2 scores (negated for convenience)
#PrincipalComponent4 <- -1*pc.comp[,4] # principal component 2 scores (negated for convenience)
#PrincipalComponent5 <- -1*pc.comp[,5] # principal component 2 scores (negated for convenience)
#PrincipalComponent6 <- -1*pc.comp[,6] # principal component 2 scores (negated for convenience)
#PrincipalComponent7 <- -1*pc.comp[,7] # principal component 2 scores (negated for convenience)
#PrincipalComponent8 <- -1*pc.comp[,8] # principal component 2 scores (negated for convenience)
#PrincipalComponent9 <- -1*pc.comp[,9] # principal component 2 scores (negated for convenience)

clustering.data <- cbind(PrincipalComponent1, PrincipalComponent2)

library(cluster)
library(ggplot2)
library(factoextra)
library(colorspace)
library(dendextend)

# K-Mean Clustering
set.seed(100000)
km <- kmeans(clustering.data, 10, iter.max = 30000, nstart=30)
km
km$cluster
#plot(PrincipalComponent1, PrincipalComponent2, col=km$cluster)
#points(km$centers, pch=16)

fviz_cluster(km, data = clustering.data)

aggregate(yeast[, 2:9],by=list(km$cluster),mean)
table(km$cluster, yeast$LocalizationSite)

#Spectral Clustering
set.seed(10000)
library(kknn)
cl   <- specClust(clustering.data, centers=10, nn=50, iter.max=10000) 
cl
#plot(PrincipalComponent1, PrincipalComponent2, col=cl$cluster)

table(cl$cluster, yeast$LocalizationSite)

aggregate(yeast[, 2:9],by=list(cl$cluster),mean)

fviz_cluster(cl, data = clustering.data)

#Hierarchical Clustering

library(cluster)
library(ggplot2)
library(factoextra)
library(colorspace)
library(dendextend)

d_yeast<- dist(clustering.data)
hclusters <- hclust(d_yeast, method = "average")
clusterCut <- cutree(hclusters, 10)
clusterCut
table(clusterCut, yeast$LocalizationSite)
aggregate(yeast[, 2:9],by=list(clusterCut),mean)



#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)


dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 10) %>% plot(main = "Average Cluster Dendrogram")
dend %>% rect.dendrogram(k=10, border = 8, lty = 5, lwd = 2)

# Horizontal plot
dend %>% set("branches_k_color", k = 10) %>% plot(main = "Average Cluster Dendrogram",horiz = TRUE )
dend %>% rect.dendrogram(k = 10, horiz = TRUE, border = 8, lty = 5, lwd = 2)

#Single link

library(cluster)
library(ggplot2)
library(factoextra)
library(colorspace)
library(dendextend)

d_yeast<- dist(clustering.data)
hclusters <- hclust(d_yeast, method = "single")
clusterCut <- cutree(hclusters, 10)
clusterCut
table(clusterCut, yeast$LocalizationSite)
aggregate(yeast[, 2:9],by=list(clusterCut),mean)


dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 10) %>% plot(main = "Single Cluster Dendrogram")
dend %>% rect.dendrogram(k=10, border = 8, lty = 5, lwd = 2)

# Horizontal plot
dend %>% set("branches_k_color", k = 10) %>% plot(main = "Single Cluster Dendrogram",horiz = TRUE )
dend %>% rect.dendrogram(k = 10, horiz = TRUE, border = 8, lty = 5, lwd = 2)




#Complete link
library(cluster)
library(ggplot2)
library(factoextra)
library(colorspace)
library(dendextend)

d_yeast<- dist(clustering.data)
hclusters2 <- hclust(d_yeast, method = "complete")
clusterCut <- cutree(hclusters2, 10)
clusterCut
table(clusterCut, yeast$LocalizationSite)
aggregate(yeast[, 2:9],by=list(clusterCut),mean)


dend<- as.dendrogram(hclusters2)
# Vertical plot
dend %>% set("branches_k_color", k = 10) %>% plot(main = "Complete Cluster Dendrogram")
dend %>% rect.dendrogram(k=10, border = 8, lty = 5, lwd = 2)

# Horizontal plot
dend %>% set("branches_k_color", k = 10) %>% plot(main = "Complete Cluster Dendrogram",horiz = TRUE )
dend %>% rect.dendrogram(k = 10, horiz = TRUE, border = 8, lty = 5, lwd = 2)
