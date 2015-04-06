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

levels <- c(0:9)
cutoffs <- c(0.8, 0.9, 0.95, 0.99);
count <- 0;
folds <- getFolds(data = myData.data, classes = myData.data.class)

# Starts new plot
png("Img/MYDATA_NB_PCA_BIN.png")
for(i in 1:length(cutoffs))
{
  count <- count + 1;
  cutoff <- cutoffs[[i]];
  print(paste("Running cutoff: ", cutoff))
  
  myData.data.pca <- pcaTruncate(data = myData.data, cutoff = cutoff)
  
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
    
    cutpoints <- quantile(training, (0:bins) / bins)
    
    myData.binned <- cut(training, cutpoints, include.lowest=TRUE)
    myData.binned <- split(myData.binned, 1:ncol(training))
    myData.binned <- as.data.frame(myData.binned)
    
    myData.binned.test <- cut(testing, cutpoints, include.lowest=TRUE)
    myData.binned.test <- split(myData.binned.test, 1:ncol(testing))
    myData.binned.test <- as.data.frame(myData.binned.test)
    
    myData.naive.model <- naiveBayes(x = myData.binned, y = trainClassF)
    
    myData.naive.predict <- predict(myData.naive.model, myData.binned.test)
    
    myData.naive.table <- table(myData.naive.predict, testClassF)
    myData.table.units <- table(myData.naive.predict == testClassF, testClassF)[seq(from = 2, to = 20, by = 2)]
    myData.table.prob <- myData.table.units / (nrow(testing) / nlevels(testClassF))
    
    result <- c(result, myData.table.prob)
  }
  # Calculate the mean correctness for each digit. 
  correctness <- c()
  for(digit in 1:10) {
    index.digit <- seq(from = digit, to = length(result), by = 10);
    avg <- mean(result[index.digit])
    correctness <- c(correctness, avg)
  }
  
  if(count == 1)
  {
    plot(x = levels, y = correctness, col = count, type = "b", xlim = c(0, 9), ylim = c(0, 1), ylab = 'Success rate', xlab = 'Digits', main = "Naive Bayes with PCA for digits Success Rate") 
  }
  else
  {
    lines(x = levels, y = correctness, col = count, type = "b")
  }
  
}
legend(x = 5, y = 0.3, inset=.05, title="Cutoff Points", fill=seq(1, count), legend = cutoffs)
dev.off()

# # # #
# Running for All Data
# # # # 

#Path to images
ressourcePath <- 'C:/Users/wisienkas/workspace/SML-database';
allData.path <- getAllMemberImages(basePath = ressourcePath)

#The raw data
allData.data.raw <- loadAllPeople(basePath = ressourcePath)

allData.data <- streamlineList(largeList = allData.data.raw);

allData.data.class <- classification(numbers = rep(times = nrow(allData.data) / 4000, x = 0:9), times = 400)

levels <- c(0:9)
cutoffs <- c(0.8, 0.9, 0.95, 0.99)
count <- 0
folds <- getFolds(data = allData.data, classes = allData.data.class)
bins <- 10

# Starts new plot
png("Img/ALLDATA_NB_PCA_BIN.png")
for(i in 1:length(cutoffs))
{
  count <- count + 1;
  cutoff <- cutoffs[[i]];
  print(paste("Running cutoff: ", cutoff))
  
  allData.data.pca <- pcaTruncate(data = allData.data, cutoff = cutoff)
  
  result <- c()
  # 10 runs of crossvalidation
  for(cv in 1:10) {
    print(paste("Running in run:", cv, " out of:", 10))
    testFold.range <- folds[[cv]];
    testFold.index <- getIndexFromFolds(testFold.range$from, testFold.range$to)
    testing <- allData.data.pca[testFold.index, ]
    testClassF <- allData.data.class[testFold.index]
    training <- allData.data.pca[-testFold.index, ]
    trainClassF <- allData.data.class[-testFold.index]
    
    cutpoints <- quantile(training, (0:bins) / bins)
    
    allData.binned <- cut(training, cutpoints, include.lowest=TRUE)
    allData.binned <- split(allData.binned, 1:ncol(training))
    allData.binned <- as.data.frame(allData.binned)
    
    allData.binned.test <- cut(testing, cutpoints, include.lowest=TRUE)
    allData.binned.test <- split(allData.binned.test, 1:ncol(testing))
    allData.binned.test <- as.data.frame(allData.binned.test)
    
    allData.naive.model <- naiveBayes(x = allData.binned, y = trainClassF)
    
    allData.naive.predict <- predict(allData.naive.model, allData.binned.test)
    
    allData.naive.table <- table(allData.naive.predict, testClassF)
    allData.table.units <- table(allData.naive.predict == testClassF, testClassF)[seq(from = 2, to = 20, by = 2)]
    allData.table.prob <- allData.table.units / (nrow(testing) / nlevels(testClassF))
    
    result <- c(result, allData.table.prob)
  }
  # Calculate the mean correctness for each digit. 
  correctness <- c()
  for(digit in 1:10) {
    index.digit <- seq(from = digit, to = length(result), by = 10);
    avg <- mean(result[index.digit])
    correctness <- c(correctness, avg)
  }
  
  if(count == 1)
  {
    plot(x = levels, y = correctness, col = count, type = "b", xlim = c(0, 9), ylim = c(0, 1), ylab = 'Success rate', xlab = 'Digits', main = "Naive Bayes with PCA for digits Success Rate") 
  }
  else
  {
    lines(x = levels, y = correctness, col = count, type = "b")
  }
  
}
legend(x = 5, y = 1, inset=.05, title="Cutoff Points", fill=seq(1, count), legend = cutoffs)
dev.off()