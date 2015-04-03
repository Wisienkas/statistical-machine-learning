source('file_locator.r')
source('image_reader.r')
source('knn_runner.r')
source('normalization.r')
source('knn_cross_validation.r')
source('pca_optimization.r')

library(klaR)
library(e1071)
library(caret)

#Path to images
ressourcePath <- 'E:\\SML_SVN\\trunk';
myData.path <- getSingleMemberImages(basePath = ressourcePath, 'group6', 'member2');

#The raw data
myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 0, DPI = 100);
#allData.data.raw <- loadAllPeople(basePath = ressourcePath)

retList = getRandomSplit(myData.data.raw, 0.9)
#retList =getRandomSplit(allData.data.raw, 0.9);
training = retList[[1]]
testing = retList[[2]]
trainClassF = retList[[3]]
testClassF = retList[[4]]

levels <- c(0:9)
binArr <- c(2,5,10,15);
count <- 0;

for(i in 1:length(binArr))
{
  count <- count + 1;
  bins <- binArr[[i]];
  cutpoints <- quantile(training, (0:bins) / bins)
  
  myData.binned <- cut(training, cutpoints, include.lowest=TRUE)
  myData.binned <- split(myData.binned, 1:380)
  myData.binned <- as.data.frame(myData.binned)
  
  myData.binned.test <- cut(testing, cutpoints, include.lowest=TRUE)
  myData.binned.test <- split(myData.binned.test, 1:380)
  myData.binned.test <- as.data.frame(myData.binned.test)
  
  myData.naive.model <- naiveBayes(x = myData.binned, y = trainClassF)
  
  myData.naive.predict <- predict(myData.naive.model, myData.binned.test)
  
  myData.naive.table <- table(myData.naive.predict, testClassF)
  myData.table.units <- table(myData.naive.predict == testClassF, testClassF)[seq(from = 2, to = 20, by = 2)]
  myData.table.prob <- myData.table.units / (nrow(testing) / nlevels(testClassF))
    
  if(count == 1)
  {
    plot(x = levels, y = myData.table.prob, type = "b", col=count, xlim = c(0, 9), ylim = c(0, 1), ylab = 'Success rate', xlab = 'Digits')
  }
  else
  {
    lines(x = levels, y = myData.table.prob, type = "b", col=count)    
  }
}
legend("topright", inset=.05, title="Number of bins", fill=seq(1, count), bty = 'n', legend = binArr)



