install.packages("randomForest")

library("randomForest")
library(randomForest)
source('file_locator.r')
source('image_reader.r')
source('training_generator.r')

ressourcePath <- 'E:\\SML_SVN\\trunk';
myData.path <- getSingleMemberImages(basePath = ressourcePath, 'group6', 'member2');

#The raw data
myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 0, DPI = 100);
#allData.data.raw <- loadAllPeople(basePath = ressourcePath)

myData.data <- streamlineList(largeList = myData.data.raw);

myData.data.class <- classification(numbers = 0:9, times = 400)

retList = getRandomSplit(myData.data.raw, 0.9)
training = retList[[1]]
testing = retList[[2]]
trainClassF = retList[[3]]
testClassF = retList[[4]]

rf <- randomForest(x= training, y = trainClassF);

print(rf)
importance(rf)
plot(rf)
plot( importance(rf), lty=2, pch=16)
lines(importance(rf))
imp = importance(rf)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(1, 3))

#NO CLUE WHAT I AM DOING!