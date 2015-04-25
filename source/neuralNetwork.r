# install, load package
install.packages("NeuralNetTools")
install.packages("neuralnet")

library(NeuralNetTools)
library(neuralnet)

source("file_locator.r")
source("image_reader.r")
source("training_generator.r")

makeClassBinaryTable <- function(classes) {
  result <- data.frame("class0" = classes,
                       "class1" = classes,
                       "class2" = classes,
                       "class3" = classes,
                       "class4" = classes,
                       "class5" = classes,
                       "class6" = classes,
                       "class7" = classes,
                       "class8" = classes,
                       "class9" = classes)
  for(i in 0:9) {
    result[,i + 1] <- result[,i + 1] == i
  }
  result[result == TRUE] = 1
  
  return(result)
}

splitMatrix <- function(matrix, split) {
  splits <- split * nrow(matrix)
  trainRows <- sample(1:nrow(matrix), size = splits, replace = FALSE)
  
  trainData <- matrix[trainRows, ];
  testData <- matrix[-trainRows, ];
  
  return(list("train" = trainData, "test" = testData))
}

# Loading the data 
basePath <- "/home/wisienkas/workspace/school/6sem/sml/exercise/svn/trunk"
myData.rawData.path <- getSingleMemberImages(basePath = basePath, group_name = "group6", member_name = "member1")
myData.rawData <- loadPersonsImageData(memberInfo = myData.rawData.path, sigma = 1, DPI = 100)

myData.data <- streamlineList(largeList = myData.rawData)
myData.class <- classification(numbers = 0:9, times = nrow(myData.data) / 10)

myData.pca <- pcaTruncate(data = myData.data, cutoff = 0.95)

# Map Everything
myData.classMatrix <- makeClassBinaryTable(myData.class)
myData.map <- cbind(myData.pca, myData.classMatrix)

myData.colnames.data <- colnames(myData.pca)
myData.colnames.class <- colnames(myData.classMatrix)

# Getting Invalid Formula here. hmmm
names <- c(colnames(myData.pca), colnames(myData.classMatrix))
colnames(myData.map) <- names;

splitMatrix()

myData.formula <- paste(paste(myData.colnames.class, collapse = " + "), paste(myData.colnames.data, collapse = " + "), sep = " ~ ")
mod <- neuralnet(myData.formula, data = myData.map[seq(from = 1, to = 4000, by = 10),], hidden = 380, threshold = 0.01, rep = 1)

# plotnet
par(mar = numeric(4), family = 'serif')
plotnet(mod, alpha = 0.6)

