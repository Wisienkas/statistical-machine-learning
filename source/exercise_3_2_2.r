source('file_locator.r')
source('image_reader.r')
source('knn_runner.r')
source('normalization.r')
source('knn_cross_validation.r')
source('pca_optimization.r')

library(klaR)
library(e1071)
library(caret)

#######
# FOR MY DATA
#######

#Path to images
ressourcePath <- 'C:/Users/wisienkas/workspace/SML-database';
myData.path <- getSingleMemberImages(basePath = ressourcePath, 'group6', 'member2');

#The raw data
myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 0, DPI = 100);

myData.data <- streamlineList(largeList = myData.data.raw)

levels <- c(0:9)
bins <- 5;

folds <- getFolds(data = myData.data, classes = myData.data.class)

count <- 0;
laplaceArr <- c(0, 1, 5, 50, 300)

# Creating PNG Image putput
png("Img/MYDATA_NB_LAPLACE.png")
for(i in 1:length(laplaceArr))
{
  laplaceS <- laplaceArr[[i]];
  count <- count + 1;
  print(paste("Running Laplace:", laplaceS))
  
  result <- c()
  # 10 runs of crossvalidation
  for(cv in 1:10) {
    print(paste("Running in run:", cv, " out of:", 10))
    testFold.range <- folds[[cv]];
    testFold.index <- getIndexFromFolds(testFold.range$from, testFold.range$to)
    testing <- myData.data[testFold.index, ]
    testClassF <- myData.data.class[testFold.index]
    training <- myData.data[-testFold.index, ]
    trainClassF <- myData.data.class[-testFold.index]
    
    cutpoints <- quantile(training, (0:bins) / bins)
    
    myData.binned <- cut(training, cutpoints, include.lowest=TRUE)
    myData.binned <- split(myData.binned, 1:380)
    myData.binned <- as.data.frame(myData.binned)
    
    myData.binned.test <- cut(testing, cutpoints, include.lowest=TRUE)
    myData.binned.test <- split(myData.binned.test, 1:380)
    myData.binned.test <- as.data.frame(myData.binned.test)
    myData.naive.model <- naiveBayes(x = myData.binned, y = trainClassF, laplace = laplaceS)
    
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
    plot(x = levels, y = correctness, type = "b", col=count, xlim = c(0,9), ylim = c(0, 1), ylab = 'Success rate', xlab = 'Digits')
  }
  else
  {
    lines(x = levels, y = correctness, type = "b", col=count)
  }  
}
legend("topright", inset=.05, title="Laplace smoothing", fill=seq(1, count), bty = 'n', legend = laplaceArr)
dev.off()

#######
# FOR ALL DATA
#######

#Path to images
ressourcePath <- 'C:/Users/wisienkas/workspace/SML-database';
allData.data.raw <- loadAllPeople(basePath = ressourcePath)

allData.data <- streamlineList(largeList = allData.data.raw)

levels <- c(0:9)
bins <- 10;
folds <- getFolds(data = allData.data, classes = allData.data.class)

count <- 0;
laplaceArr <- c(0, 1, 5, 50, 300)

# Creating PNG Image putput
png("Img/ALLDATA_NB_LAPLACE.png")
for(i in 1:length(laplaceArr))
{
  laplaceS <- laplaceArr[[i]];
  count <- count + 1;
  print(paste("Running Laplace:", laplaceS))
  
  result <- c()
  # 10 runs of crossvalidation
  for(cv in 1:10) {
    print(paste("Running in run:", cv, " out of:", 10))
    testFold.range <- folds[[cv]];
    testFold.index <- getIndexFromFolds(testFold.range$from, testFold.range$to)
    testing <- allData.data[testFold.index, ]
    testClassF <- allData.data.class[testFold.index]
    training <- allData.data[-testFold.index, ]
    trainClassF <- allData.data.class[-testFold.index]
    
    cutpoints <- quantile(training, (0:bins) / bins)
    
    allData.binned <- cut(training, cutpoints, include.lowest=TRUE)
    allData.binned <- split(allData.binned, 1:380)
    allData.binned <- as.data.frame(allData.binned)
    
    allData.binned.test <- cut(testing, cutpoints, include.lowest=TRUE)
    allData.binned.test <- split(allData.binned.test, 1:380)
    allData.binned.test <- as.data.frame(allData.binned.test)
    allData.naive.model <- naiveBayes(x = allData.binned, y = trainClassF, laplace = laplaceS)
    
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
    plot(x = levels, y = correctness, type = "b", col=count, xlim = c(0,9), ylim = c(0, 1), ylab = 'Success rate', xlab = 'Digits')
  }
  else
  {
    lines(x = levels, y = correctness, type = "b", col=count)
  }  
}
legend("topright", inset=.05, title="Laplace smoothing", fill=seq(1, count), bty = 'n', legend = laplaceArr)
dev.off()