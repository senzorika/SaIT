# ============================================================
# Exercise 8: Cluster analysis
# Theory: https://senzorika.github.io/SaIT/theory_EN/exercise08.html
# ============================================================

#------------------------------------------------------------
# Cluster analysis (hierarchical) and product categorisation
# -----------------------------------------------------------

# load the dataset from an internet address
sensory <- read.table("http://senzorika.com/sait/datasety/senzorika.txt", sep = ",")
names <- c("pizza_1", "pizza_2", "pizza_3", "pizza_4", "pizza_5", "pizza_6", "pizza_7", "pizza_8", "pizza_9", "pizza_10", "pizza_11", "pizza_12", "pizza_13", "pizza_14", "pizza_15", "pizza_16")
row.names(sensory) <- names
# Ward's hierarchical clustering
d <- dist(sensory, method = "euclidean") # distance matrix
fit <- hclust(d, method = "ward.D")
plot(fit, main = "Product dendrogram", ylab = "distance (merge height)", xlab = "Products") # display the dendrogram
groups <- cutree(fit, k = 3) # cut the tree into three groups
rect.hclust(fit, k = 3, border = "red")



# -----------------------------------------------------------
# k-means clustering
#------------------------------------------------------------
# load the required packages
library(tidyverse) # data manipulation
library(cluster) # clustering algorithms
library(factoextra) # clustering visualisation

# load the data
sensory <- read.table("http://senzorika.com/sait/datasety/senzorika.txt", sep = ",")
distance <- get_dist(sensory)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(sensory, centers = 2, nstart = 25)
k3 <- kmeans(sensory, centers = 3, nstart = 25)
k5 <- kmeans(sensory, centers = 5, nstart = 25)
fviz_cluster(k2, data = sensory)
fviz_cluster(k5, geom = "point", data = sensory) + ggtitle("k = 5")

# optimal number of clusters
fviz_nbclust(sensory, kmeans, method = "silhouette")
# visualisation with three clusters
fviz_cluster(k3, data = sensory) + ggtitle("Optimal segmentation of products into groups")
cor(k3$centers)
k3$centers
k3$size

# Example:
# A sensory panel evaluated our product A against 15 competing products on the market. The aim was to find out
# which product is most similar to ours when the panel divided the whole assortment into 5 groups. Comment on the results.

sensory <- read.table("http://senzorika.com/sait/datasety/senzorika.txt", sep = ",")
names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "X")
