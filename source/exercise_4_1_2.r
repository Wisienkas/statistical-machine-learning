install.packages('tree');

library(tree)

source('file_locator.r')
source('image_reader.r')
source('training_generator.r')

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

retList = getRandomSplit(allData.data.raw, 0.9)
training = retList[[1]]
testing = retList[[2]]
trainClassF = retList[[3]]
testClassF = retList[[4]]

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




#NO CLUE WHAT I AM DOING!