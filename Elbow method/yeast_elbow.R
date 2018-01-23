yeast <- read.table(url("http://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.data"), header = FALSE)

names(yeast)<- c("SequenceName", "mcg", "gvh", "alm", "mit", "erl", "pox", "vac", "nuc", "LocalizationSite")

#yeast1 <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/yeast/yeast.data", sep = ",")


if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")

pkgs <- c("cluster",  "NbClust")
#install.packages(pkgs)

library(factoextra)
library(cluster)
library(NbClust)
library(mixOmics)
library(mlbench)
# Load the data
#data("yeast")
#head(yeast)
yeast1 <-  yeast[ ,2:9]
yeast.scaled <- scale(yeast1)
#yeast.scaled <- scale(yeast1[c(' mcg','gvh','alm','mit','erl','pox','vac','nuc')])
#yeast.scaled <- scale(yeast[c(' mcg','gvh','alm','mit','erl','pox','vac','nuc')])
# Remove any missing value (i.e, NA values for not available)
# That might be present in the data
df <- na.omit(yeast1)
# View the firt 6 rows of the data
head(df, n = 6)
head(yeast1)
desc_stats <- data.frame(
  Min = apply(df, 2, min), # minimum
  Med = apply(df, 2, median), # median
  Mean = apply(df, 2, mean), # mean
  SD = apply(df, 2, sd), # Standard deviation
  Max = apply(df, 2, max) # Maximum
)
desc_stats <- round(desc_stats, 1)
head(desc_stats)

df <- scale(df)
head(df)

library(factoextra)
set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")+ 
  geom_vline(xintercept = 7, linetype = 2)
##


set.seed(123)
km.res <- kmeans(yeast.scaled, 6, nstart = 25)

# k-means group number of each observation
km.res$cluster

# Visualize k-means clusters
fviz_cluster(km.res, data = yeast.scaled, geom = "point",
             stand = FALSE, frame.type = "norm")
