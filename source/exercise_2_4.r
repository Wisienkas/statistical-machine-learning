# Libraries Needed
source("file_locator.r")
source("image_reader.r")
source("pca_optimization.r")
source("training_generator.r")
source("knn_cross_validation.r")
library("FactoMineR")

# Read in Data
ressourcePath <- 'C:/Users/wisienkas/workspace/SML-database'

# Get the paths for independent data
myData.path <- getSingleMemberImages(basePath = ressourcePath, 'group6', 'member1');

# Load in Data for both single person and all persons
myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 1, DPI = 100)
allData.data.raw <- loadAllPeople(basePath = ressourcePath)

myData.data <- streamlineList(myData.data.raw)
allData.data <- streamlineList(allData.data.raw)

myData.classF <- classification(numbers = seq(0:9), times = 400)
allData.classF <- rep(x = myData.classF, times = ( dim(allData.data)[1] / dim(myData.data)[1] ) )

myData.trunc <- pcaTruncate(data = myData.data, cutoff = 0.95)
allData.trunc <- pcaTruncate(data = allData.data, cutoff = 0.95)

myData.kmeans <- kmeans(myData.trunc, centers = 10, iter.max = 30)
allData.kmeans <- kmeans(allData.trunc, centers = 140, iter.max = 30)

# 2D Clustering Image
plot(x = myData.data[1:2, ], col = myData.kmeans$cluster, pch = 20, cex = 2, main = "K means Clustering")
# 3D Clustering Image
scatterplot3d(myData.trunc[,1:3], color = myMeans$cluster, pch = 1, cex.symbols = 0.5, main = "K means Clustering")
# Hierarchy Complete
myData.complete <- hclust(dist(myData.trunc), method = "complete")
plot(myData.complete, main = "Hierarchy Complete", cex = 0.1)
# Hierarchy Single
myData.single <- hclust(dist(myData.trunc), method = "single")
plot(myData.single, main = "Hierarchy Single", cex = 0.1)

