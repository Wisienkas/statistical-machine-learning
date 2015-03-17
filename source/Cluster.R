# Libraries
library(scatterplot3d)
attach(mtcars)

get2dCluster <- function(cluster, data) {
  plot(x = data[,1:2], col = cluster$cluster, pch = 20, cex = 2, main = "K means Clustering")
}

get3dCluster <- function(cluster, data) {
  scatterplot3d(data[,1:3], color = cluster$cluster, pch = 1, cex.symbols = 0.5, main = "K means Clustering")
}

getCompleteHiearchy <- function(data) {
  complete <- hclust(dist(data), method = "complete")
  plot(data, main = "Hierarchy Complete", cex = 0.1)
}

getSingleHierarchy <- function(data) {
  single <- hclust(dist(data), method = "single")
  plot(single, main = "Hierarchy Single", cex = 0.1)
}