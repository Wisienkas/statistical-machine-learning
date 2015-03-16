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

allData.kmeans <- kmeans(allData.data, centers = 140, iter.max = 30) 
