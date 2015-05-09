source('file_locator.r')
source('image_reader.r')
source('knn_runner.r')
source('normalization.r')
source('knn_cross_validation.r')
source('pca_optimization.r')
source('training_generator.r')

install.packages("kernlab")
library(kernlab)
library(klaR)
library(caret)
example(ksvm)


# # # #
# Running for My Data
# # # # 

#Path to images
ressourcePath <- 'C:/Users/wisienkas/workspace/SML-database';
myData.path <- getSingleMemberImages(basePath = ressourcePath, 'group6', 'member2');

#The raw data
myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 0, DPI = 100);
#allData.data.raw <- loadAllPeople(basePath = ressourcePath)

myData.data <- streamlineList(largeList = myData.data.raw);

myData.data.class <- classification(numbers = 0:9, times = 400)

dataSplit <- splitBalanced(largeMatrix = myData.data, myData.data.class, split = 0.9);
train <- dataSplit[[1]]
trainC <- dataSplit[[3]]
test <- dataSplit[[2]]
testC <- dataSplit[[4]]


# Setting labels for formula
colnames(train) <- c(paste("col", 1:(ncol(train) - 1), sep = ""), "class")
colnames(test) <- c(paste("col", 1:(ncol(test)), sep = ""))

SupportVector <- ksvm(x = train, data = NULL, y = trainC, kernel="rbfdot", kpar="automatic",C = 3, prob.model=FALSE)

result <- predict(SupportVector, test)

result.table <- table(result, testC)
error <- mean(result != testC)
sum(result == factor(dataSplit[[4]], levels = 0:9)) / ncol(test)

