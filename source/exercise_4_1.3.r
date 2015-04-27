install.packages('tree');

library(tree)

source('file_locator.r')
source('image_reader.r')
source('knn_runner.r')
source('normalization.r')
source('knn_cross_validation.r')
source('pca_optimization.r')
source('training_generator.r')

library(klaR)
library(e1071)
library(caret)

# Loading the data
# Nikolaj PATHs: 
# Home pc:  C:/Users/wisienkas/workspace/SML-database
# Laptop :  /home/wisienkas/workspace/school/6sem/sml/exercise/svn/trunk
#
# Thomas PATHs:
# home pc: 
# Laptop : 
basePath <- "/home/wisienkas/workspace/school/6sem/sml/exercise/svn/trunk"
myData.rawData.path <- getSingleMemberImages(basePath = basePath, group_name = "group6", member_name = "member1")
myData.data.raw <- loadPersonsImageData(memberInfo = myData.rawData.path, sigma = 1, DPI = 100)

allData.data.raw <- loadAllPeople(basePath = basePath)

#The raw data
#myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 0, DPI = 100);
#allData.data.raw <- loadAllPeople(basePath = ressourcePath)

myData.data <- streamlineList(largeList = myData.data.raw);

myData.data.class <- classification(numbers = 0:9, times = 400)

folds <- getFolds(data = myData.data, classes = myData.data.class)

myData.data.pca <- pcaTruncate(data = myData.data, cutoff = 0.95)

result <- c()
# 10 runs of crossvalidation
for(cv in 1:10) {
  print(paste("Running in run:", cv, " out of:", 10))
  testFold.range <- folds[[cv]];
  testFold.index <- getIndexFromFolds(testFold.range$from, testFold.range$to)
  testing <- myData.data.pca[testFold.index, ]
  testClassF <- myData.data.class[testFold.index]
  training <- myData.data.pca[-testFold.index, ]
  trainClassF <- myData.data.class[-testFold.index]
  
  data <- cbind(training, trainClassF)
  data[, 381] <- data[, 381] - 1 
  
  #Generate the descision tree
  stree = tree(factor(trainClassF) ~., data = as.data.frame(data))
  
  #Plot the tre
  plot(stree)
  text(stree, pretty = 0)
  
  stree.predict = predict(stree, as.data.frame(testing), type = "class")
  
  stree.table <- table(stree.predict, testClassF)
  correctness <- mean(stree.predict != testClassF)
  
  result <- c(result, myData.table.prob)
}
