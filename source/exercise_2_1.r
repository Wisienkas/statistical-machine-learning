# Libraries Needed
source("file_locator.r")
source("image_reader.r")
source("pca_optimization.r")
source("training_generator.r")
library("FactoMineR")

# Read in Data
ressourcePath <- 'C:/Users/wisienkas/workspace/SML-database'

# Get the paths for independent data
myData.path <- getSingleMemberImages(basePath = ressourcePath, 'group6', 'member1');

# Load in Data for both single person and all persons
myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 1, DPI = 100)
allData.data.raw <- loadAllPeople(basePath = ressourcePath)

myData.data <- streamlineList(myData.data.raw)
allData.data <- streamlineList(allData.data.raw)

myData.classF <- classification(numbers = seq(0:9), times = 400)
allData.classF <- rep(x = myData.classF, times = ( dim(allData.data)[1] / dim(myData.data)[1] ) )

# 2.1.2  Show the eigenvalues, variance and the accumulated variance 
# of the principal components. (In the report 10-20 values 
# equally distributed, should be sufficient to illustrate the tendencies.) 
plot(PCA(myData.data, graph = FALSE)$eig, col = 3)
plot(PCA(allData.data, graph = FALSE)$eig, col = 2)

# 2.1.3  Show the performance of the K-nearest neighbor algorithm when selecting 
# enough principal components to represent 80%, 90%, 95%, 99% of 
# the accumulated variance. For each test vary K.
cutoff_arr <- c(0.8, 0.9, 0.95, 0.99)
k_arr <- c(1, 3, 7, 13)

cv_runs <- 10

result <- NULL

for(cutoff in cutoff_arr) {
  myData.trunc <- pcaTruncate(data = myData.data, cutoff = cutoff)
  #allData.trunc <- pcaTruncate(data = allData.data, cutoff = cutoff)
  for(k in k_arr) {
    temp <- knnCrossValidation(RawTrainData = myData.trunc, runs = cv_runs, k = k)
    temp <- c(paste("K:", k), paste("cutoff:" , cutoff), "MyData", temp)
    result <- rbind(result, temp)
    #temp <- knnCrossValidation(RawTrainData = allData.trunc, runs = cv_runs, k = k)
    #temp <- c(paste("K:", k), paste("cutoff:" , cutoff), "MyData", temp)
    #result <- rbind(result, temp)
  }
}