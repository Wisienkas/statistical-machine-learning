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

# Starts new plot
png("Img/MYDATA_NB_PCA.png")
for(i in 1:length(cutoffs))
{
  count <- count + 1;
  cutoff <- cutoffs[[i]];
  
  myData.data.pca <- pcaTruncate(data = myData.data, cutoff = cutoff)
  
  retList = splitBalanced(largeMatrix = myData.data, classes = myData.data.class, split = 0.9)
  training = retList[[1]]
  testing = retList[[2]]
  trainClassF = retList[[3]]
  testClassF = retList[[4]]
  
  myData.naive.model <- naiveBayes(x = training, y = trainClassF)
  
  myData.naive.predict <- predict(myData.naive.model, testing)
  
  myData.naive.table <- table(myData.naive.predict, testClassF)
  myData.table.units <- table(myData.naive.predict == testClassF, testClassF)[seq(from = 2, to = 20, by = 2)]
  myData.table.prob <- myData.table.units / (nrow(testing) / nlevels(testClassF))
  
  if(count == 1)
  {
    plot(x = levels, y = myData.table.prob, col = count, type = "b", xlim = c(0, 9), ylim = c(0, 1), ylab = 'Success rate', xlab = 'Digits', main = "Naive Bayes with PCA for digits Success Rate") 
  }
  else
  {
    lines(x = levels, y = myData.table.prob, col = count, type = "b")
  }
}
legend(x = 5, y = 0.3, inset=.05, title="Number of bins", fill=seq(1, count), legend = cutoffs)
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
cutoffs <- c(0.8, 0.9, 0.95, 0.99);
count <- 0;

# Starts new plot
png("Img/ALLDATA_NB_PCA.png")
for(i in 1:length(cutoffs))
{
  count <- count + 1;
  cutoff <- cutoffs[[i]];
  
  allData.data.pca <- pcaTruncate(data = allData.data, cutoff = cutoff)
  
  retList = splitBalanced(largeMatrix = allData.data, classes = allData.data.class, split = 0.9)
  training = retList[[1]]
  testing = retList[[2]]
  trainClassF = retList[[3]]
  testClassF = retList[[4]]
  
  allData.naive.model <- naiveBayes(x = training, y = trainClassF)
  
  allData.naive.predict <- predict(allData.naive.model, testing)
  
  allData.naive.table <- table(allData.naive.predict, testClassF)
  allData.table.units <- table(allData.naive.predict == testClassF, testClassF)[seq(from = 2, to = 20, by = 2)]
  allData.table.prob <- allData.table.units / (nrow(testing) / nlevels(testClassF))
  
  if(count == 1)
  {
    plot(x = levels, y = allData.table.prob, col = count, type = "b", xlim = c(0, 9), ylim = c(0, 1), ylab = 'Success rate', xlab = 'Digits', main = "Naive Bayes with PCA for digits Success Rate") 
  }
  else
  {
    lines(x = levels, y = allData.table.prob, col = count, type = "b")
  }
}
legend(x = 5, y = 1, inset=.05, title="Cutoff Points", fill=seq(1, count), legend = cutoffs)
dev.off()