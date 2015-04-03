
binData <- function(data, bins) {
  cutpoints <- quantile(data, (0:bins) / bins)
  
  binned
}

source('file_locator.r')
source('image_reader.r')
source('knn_runner.r')
source('normalization.r')
source('knn_cross_validation.r')
source('pca_optimization.r')

#Path to images
myData.path <- getSingleMemberImages('/home/wisienkas/workspace/school/6sem/sml/exercise/svn/trunk', 'group6', 'member2');

#The raw data
myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 0, DPI = 100);

retList = getRandomSplit(myData.data.raw, 0.9)
training = retList[[1]]
testing = retList[[2]]
trainClassF = retList[[3]]
testClassF = retList[[4]]

bins <- 5
cutpoints <- quantile(training, (0:bins) / bins)

myData.binned <- cut(training, cutpoints, include.lowest=TRUE)
myData.binned <- split(myData.binned, 1:380)
myData.binned <- as.data.frame(myData.binned)

myData.binned.test <- cut(testing, cutpoints, include.lowest=TRUE)
myData.binned.test <- split(myData.binned.test, 1:380)
myData.binned.test <- as.data.frame(myData.binned.test)

myData.naive.model <- naiveBayes(x = myData.binned, y = trainClassF)
#myData.naive.model2 <- naiveBayes(x = training, y = trainClassF)

myData.naive.predict <- predict(myData.naive.model, myData.binned.test)

myData.naive.table <- table(myData.naive.predict, testClassF)
myData.table.units <- table(myData.naive.predict == testClassF, testClassF)[seq(from = 2, to = 20, by = 2)]
myData.table.prob <- myData.table.units / (nrow(testing) / nlevels(testClassF))

levels <- c(0:9)
plot(x = levels, y = myData.table.prob, type = "l", col=1, xlim = c(0, 9), ylim = c(0, 1))
lines(x = levels, y = myData.table.prob - 0.1, type = "l", col=3)
text(x = 2, labels = c("Bin 1", "bin 2"))
