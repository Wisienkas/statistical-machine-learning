install.packages("randomForest")

library("randomForest")
library(randomForest)
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
ressourcePath <- '/home/wisienkas/workspace/school/6sem/sml/exercise/svn/trunk';
myData.path <- getSingleMemberImages(basePath = ressourcePath, 'group6', 'member2');

#The raw data
myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 0, DPI = 100);
#allData.data.raw <- loadAllPeople(basePath = ressourcePath)

myData.data <- streamlineList(largeList = myData.data.raw);

myData.data.class <- classification(numbers = 0:9, times = 400)

rf <- randomForest(x= myData.data, y = myData.data.class);

print(rf)
importance(rf)
plot(rf)
plot( importance(rf), lty=2, pch=16)
lines(importance(rf))
imp = importance(rf)
impvar = rownames(imp)[order(imp[, 1], decreasing=TRUE)]
op = par(mfrow=c(1, 3))

#NO CLUE WHAT I AM DOING!