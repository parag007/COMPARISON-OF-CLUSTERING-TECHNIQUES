data(iris)

#suppressPackageStartupMessages(library(dendextend))

library(cluster)
library(ggplot2)
library(factoextra)
library(colorspace)
library(dendextend)

pca <- princomp(iris[, 2:150], cor=T) # principal components analysis using correlation matrix
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

# K-Mean Clustering
set.seed(70000)
km <- kmeans(clustering.data, 7,iter.max = 3000,nstart=3000)
km
km$cluster
km$centers
#plot(PrincipalComponent1, PrincipalComponent2, col=km$cluster)

fviz_cluster(km, data = clustering.data)
#points(km$centers, pch=16)

aggregate(iris[, 2:10],by=list(km$cluster),mean)
table(km$cluster, iris$Type)

#Spectral Clustering
set.seed(1230000)
library(kknn)
cl   <- specClust(clustering.data, centers=7, nn=50, iter.max = 300000) 
cl
#pcol <- as.character(glass$Type)
#pairs(glass[2:10], pch = pcol, col = c("green", "red")[cl$cluster])
#plot(PrincipalComponent1, PrincipalComponent2, col=cl$cluster)
#aggregate(glass[, 2:10],by=list(cl$cluster),mean)
table(cl$cluster, iris$Type)

aggregate(glass[,2:10],by=list(cl$cluster),mean)
fviz_cluster(cl, data = clustering.data)

#Hierarchical Clustering
library(ggplot2)
library(factoextra)
library(colorspace)
library(dendextend)

#set.seed(12)
d_iris<- dist(clustering.data)
hclusters <- hclust(d_iris, method = "average")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, iris$Type)
aggregate(iris[,2:10],by=list(clusterCut),mean)
plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)

#points(cl$centers, pch=16)

#cl$cluster$cluster <- as.factor(cl$cluster$cluster)
#ggplot(yeast, aes(Petal.Length, Petal.Width, color = yeast$cluster)) + geom_point()

#library(dendextend)
#dend <- as.dendrogram(hclusters)
# order it the closest we can to the order of the observations:
#dend <- rotate(dend, 1:150)

#dend <- color_branches(dend, k=10) #, groupLabels=d_yeast_species)

# Manually match the labels, as much as possible, to the real classification of the flowers:
#labels_colors(dend) <-
# rainbow_hcl(10)[sort_levels_values(
#  as.numeric(d_yeast[,8])[order.dendrogram(dend)])]

# We shall add the flower type to the labels:
#labels(dend) <- paste(as.character(d_yeast[,8])[order.dendrogram(dend)], "(",labels(dend),")", sep = "")
# We hang the dendrogram a bit:
#dend <- hang.dendrogram(dend,hang_height=0.1)
# reduce the size of the labels:
# dend <- assign_values_to_leaves_nodePar(dend, 0.5, "lab.cex")
#dend <- set(dend, "labels_cex", 0.5)
# And plot:
#par(mar = c(4,4,4,8))
#plot(dend, 
#    main = "Clustered data set", 
#    horiz =  TRUE,  nodePar = list(cex = .007))
#legend("topleft", legend = clusterCut, fill = unique(clustering.data)


dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Average Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

# Horizontal plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = " Average Cluster Dendrogram",horiz = TRUE)
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)


#Single link
d_iris<- dist(clustering.data)
hclusters <- hclust(d_iris, method = "single")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, iris$Type)
aggregate(iris[,2:10],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)

dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Single Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

# Horizontal plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Single Cluster Dendrogram",horiz = TRUE)
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)



#Complete link
d_iris<- dist(clustering.data)
hclusters <- hclust(d_iris, method = "complete")
clusterCut <- cutree(hclusters, 7)
clusterCut
table(clusterCut, iris$Type)
aggregate(iris[,2:10],by=list(clusterCut),mean)
#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)

dend<- as.dendrogram(hclusters)
# Vertical plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Complete Cluster Dendrogram")
dend %>% rect.dendrogram(k=7, border = 8, lty = 5, lwd = 2)

# Horizontal plot
dend %>% set("branches_k_color", k = 7) %>% plot(main = "Complete Cluster Dendrogram",horiz = TRUE)
dend %>% rect.dendrogram(k = 7, horiz = TRUE, border = 8, lty = 5, lwd = 2)


#fviz_cluster(hclusters, data = clustering.data)

#plot(PrincipalComponent1, PrincipalComponent2, col=clusterCut)