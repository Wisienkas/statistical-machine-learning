source('file_locator.r')
source('image_reader.r')
source('knn_runner.r')

ressourcePath <- 'C:\\subversion\\'

#My data
myData.path <- getSingleMemberImages(basePath = ressourcePath, 'group6', 'member1');
myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 0, DPI = 100)
myData.data <- streamlineList(myData.data.raw)
myData.classF <- classification(numbers = seq(0:9), times = 400)

#All data
allData.data.raw <- loadAllPeople(basePath = ressourcePath)

personer <- 14;
allData.data <- streamlineList(allData.data.raw)[1:(4000 * personer),];
#allData.data <- streamlineList(allData.data.raw);
allData.classF <- rep(x = myData.classF, times = ( dim(allData.data)[1] / dim(myData.data)[1] ) )

#Run KNN on the data
text <- c();
#text <- knn.run(largeMatrix = allData.data, classes = allData.classF, k_arr = c(1,5,10), split = 0.9);
text <- knn.run_split(largeMatrix = allData.data, classes = allData.classF, k_arr = c(1,5,10), split = 13 / 14, chunkSize = 4000)
#text <- knn.run(largeMatrix = myData.data, classes = myData.classF, k_arr = c(1,5,10), split = 0.9);
print(text)