install.packages('tree');

library(tree)
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

#Generate the descision tree
stree = tree(trainClassF ~., data = as.data.frame(training))

#Plot the tre
plot(stree)
text(stree)

stree.predict = predict(stree, as.data.frame(testing))

#NO CLUE WHAT I AM DOING!