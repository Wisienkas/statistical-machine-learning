# install, load package
install.packages("NeuralNetTools")
install.packages("neuralnet")

library(NeuralNetTools)
library(neuralnet)
library(RSNNS)

library(caret)
library(nnet)

source("file_locator.r")
source("image_reader.r")
source("training_generator.r")

source("pca_optimization.r")

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
# Nikolaj PATHs: 
# Home pc:  C:/Users/wisienkas/workspace/SML-database
# Laptop :  /home/wisienkas/workspace/school/6sem/sml/exercise/svn/trunk
#
# Thomas PATHs:
#
basePath <- "C:/Users/wisienkas/workspace/SML-database"
myData.rawData.path <- getSingleMemberImages(basePath = basePath, group_name = "group6", member_name = "member1")
myData.rawData <- loadPersonsImageData(memberInfo = myData.rawData.path, sigma = 1, DPI = 100)

myData.data <- streamlineList(largeList = myData.rawData)
myData.class <- classification(numbers = 0:9, times = nrow(myData.data) / 10)

myData.pca <- pcaTruncate(data = myData.data, cutoff = 0.95)

#myData.pca <- myData.pca[seq(from = 1, to = 4000, by = 8) ,]
#myData.class <- myData.class[seq(from = 1, to = 4000, by = 8)]

# Map Everything
myData.classMatrix <- makeClassBinaryTable(myData.class)
myData.map <- cbind(myData.pca, myData.classMatrix)

myData.colnames.data <- colnames(myData.pca)
myData.colnames.class <- colnames(myData.classMatrix)

# Getting Invalid Formula here. hmmm
names <- c(colnames(myData.pca), colnames(myData.classMatrix))
colnames(myData.map) <- names;

# Splitting with 90% on training
myData.matrix <- splitMatrix(myData.map, 0.5)

myData.test <- as.data.frame(myData.matrix["test"])
myData.train <- as.data.frame(myData.matrix["train"])

colnames(myData.test) <- names;
colnames(myData.train) <- names;

dataCols <- names[1:ncol(myData.pca)]

myData.formula <- paste(paste(myData.colnames.class, collapse = " + "), paste(myData.colnames.data, collapse = " + "), sep = " ~ ")
#myData.network <- neuralnet(myData.formula, data = myData.train, hidden = 80, threshold = 0.01, rep = 1, startweights = 0.8, stepmax = 1e6)

sizeArgs <- c(5, 15, 50, 150)
layers <- list(c(5, 5, 5), c(15, 15, 15), c(15), c(50, 15, 50))
layerNames <- c(paste("h:", paste(layers[[1]], collapse = "-")))
layerNames <- c(layerNames, paste("h:", paste(layers[[2]], collapse = "-")))
layerNames <- c(layerNames, paste("h:", paste(layers[[3]], collapse = "-")))
layerNames <- c(layerNames, paste("h:", paste(layers[[4]], collapse = "-")))

networkModel <- 1:2
for(model in networkModel) {
  
  count <- 1;
  if(model == 1) {
    png("Img/MYDATA_NN_PCA_NNET.png")
  } else {
    png("Img/MYDATA_NN_PCA_MLP.png")
  }
  for(i in 1:4) {
    if(model == 1) {
      myData.network <- nnet(x = myData.train[, dataCols], y = myData.train[, !colnames(myData.train) %in% dataCols], 
                             size = sizeArgs[i], rang = 0.01, decay = 5e-4, maxit = 10000, MaxNWts = 10000)
    } else {
      myData.network <- mlp(x = myData.train[, dataCols], y = myData.train[, !colnames(myData.train) %in% dataCols],
                            maxit = 10000, learnFuncParams=c(0.01), size = layers[[i]],
                            inputsTest = myData.test[, dataCols], targetsTest = myData.test[, !colnames(myData.train) %in% dataCols])    
    }
    
    myData.result <- predict(myData.network, myData.test[, dataCols])
    colnames(myData.result) <- 0:9
    
    myData.result.names <- colnames(myData.result)[max.col(myData.result, ties.method = "first")]
    myData.result.class <- factor(as.numeric(myData.result.names))
    
    myData.test.classM <- myData.test[, !colnames(myData.train) %in% dataCols]
    myData.test.names <- colnames(myData.test.classM)[max.col(myData.test.classM, ties.method = "first")]
    myData.test.class <- factor(as.numeric(substr(x = myData.test.names, start = 6, stop = 7)))
    
    myData.result.table <- table(myData.result.class, myData.test.class)
    myData.result.units <- table(myData.result.class == myData.test.class, myData.test.class)[seq(from = 2, to = 20, by = 2)]
    myData.table.prob <- myData.result.units / (nrow(myData.test) / nlevels(myData.test.class))
    
    if(count == 1)
    {
      plot(x = 0:9, y = myData.table.prob, col = count, type = "b", xlim = c(0, 9), ylim = c(0, 1), ylab = 'Success rate', xlab = 'Digits', main = "Neural Networks with PCA") 
    }
    else
    {
      lines(x = 0:9, y = myData.table.prob, col = count, type = "b")
    }
    count <- count + 1
  }
  grid()
  if(model == 1) {
    legend("bottomleft", inset=.05, title="hidden Nodes", fill=seq(1, count), legend = sizeArgs)
  } else {
    legend("bottomleft", inset=.05, title="hidden Nodes", fill=seq(1, count), legend = layerNames)
  }
  dev.off()
}

###
# FOR ALL DATA
### 

#The raw data
allData.data.raw <- loadAllPeople(basePath = basePath)

allData.data <- streamlineList(largeList = allData.data.raw)
allData.class <- classification(numbers = rep(times = nrow(allData.data) / 4000, x = 0:9), times = 400)

allData.pca <- pcaTruncate(data = allData.data, cutoff = 0.95)

#allData.pca <- allData.pca[seq(from = 1, to = 4000, by = 8) ,]
#allData.class <- allData.class[seq(from = 1, to = 4000, by = 8)]

# Map Everything
allData.classMatrix <- makeClassBinaryTable(allData.class)
allData.map <- cbind(allData.pca, allData.classMatrix)

allData.colnames.data <- colnames(allData.pca)
allData.colnames.class <- colnames(allData.classMatrix)

# Getting Invalid Formula here. hmmm
names <- c(colnames(allData.pca), colnames(allData.classMatrix))
colnames(allData.map) <- names;

# Splitting with 90% on training
allData.matrix <- splitMatrix(allData.map, 0.2)

allData.test <- as.data.frame(allData.matrix["test"])
allData.train <- as.data.frame(allData.matrix["train"])

colnames(allData.test) <- names;
colnames(allData.train) <- names;

dataCols <- names[1:ncol(allData.pca)]

allData.formula <- paste(paste(allData.colnames.class, collapse = " + "), paste(allData.colnames.data, collapse = " + "), sep = " ~ ")
#allData.network <- neuralnet(allData.formula, data = allData.train, hidden = 80, threshold = 0.01, rep = 1, startweights = 0.8, stepmax = 1e6)

sizeArgs <- c(5, 15, 35, 75, 200)

count <- 1;
png("Img/allData_NN_PCA.png")
for(size in sizeArgs) {
  allData.network <- nnet(x = allData.train[, dataCols], y = allData.train[, !colnames(allData.train) %in% dataCols], 
                         size = size, rang = 0.01, decay = 5e-4, maxit = 10000, MaxNWts = 10000)
  
  allData.result <- predict(allData.network, allData.test[, dataCols])
  
  allData.result.names <- colnames(allData.result)[max.col(allData.result, ties.method = "first")]
  allData.result.class <- factor(as.numeric(substr(x = allData.result.names, start = 6, stop = 7)))
  
  allData.test.classM <- allData.test[, !colnames(allData.train) %in% dataCols]
  allData.test.names <- colnames(allData.test.classM)[max.col(allData.test.classM, ties.method = "first")]
  allData.test.class <- factor(as.numeric(substr(x = allData.test.names, start = 6, stop = 7)))
  
  allData.result.table <- table(allData.result.class, allData.test.class)
  allData.result.units <- table(allData.result.class == allData.test.class, allData.test.class)[seq(from = 2, to = 20, by = 2)]
  allData.table.prob <- allData.result.units / (nrow(allData.test) / nlevels(allData.test.class))
  
  if(count == 1)
  {
    plot(x = 0:9, y = allData.table.prob, col = count, type = "b", xlim = c(0, 9), ylim = c(0, 1), ylab = 'Success rate', xlab = 'Digits', main = "Neural Networks with PCA") 
  }
  else
  {
    lines(x = 0:9, y = allData.table.prob, col = count, type = "b")
  }
  count <- count + 1
}
legend("bottomleft", inset=.05, title="hidden Nodes", fill=seq(1, count), legend = sizeArgs)
grid();
dev.off()
