source("training_generator.r")

pca_object <- function(rawData) {
  # Makes the data into 1 2d array instead of a list 
  myData <- streamlineList(rawData);
  
  levels <- seq(0:9)
  
  # Add classification for data
  myData.labels <- classification(rep(x = levels, times = length(rawData) / length(levels)), 400)
  
  # Perform PCA on data
  myData.pca <- prcomp(myData)
  
  # prcomp$sdev is Standard Deviation of principal components eigenvalue.
  # Taking the power of 2 to get eig
  myData.pca.eig <- myData.pca$sdev ^ 2
  
  totalVarMyData <- sum(myData.pca.eig)
  myData.pca.var <- myData.pca.eig / totalVarMyData
  
  
  
  return(myData)
}

streamlineList <- function(largeList) {
  print(paste("Length of large List:", length(largeList)))
  arr <- largeList[[1]]
  for(i in seq(2:length(largeList))){
    arr <- rbind(arr, largeList[[i]])
  }
  return(arr) %*%
}

haveIlligalColumns <- function(data, requiredCols) {
  for(el in data){
    if(ncol(el) != requiredCols) {
      print(paste("Illigal amount of Columns! Had:", ncol(el)))
      return(TRUE)      
    }
  }
  return(FALSE)
}

loadAllPeople <- function(basePath) {
  
  allPaths <- getAllMemberImages(basePath)
  
  illigalDataSets <- 0
  
  allData <- NULL
  # Index [1, ] is NA all over
  for(personNumber in 2:length(allPaths[, 1])) {
    temp <- loadPersonsImageData(allPaths[personNumber, ], 1, 100)
    # Handle if people doesn't have the right amount of columns as 
    # it fucks up things.
    if(haveIlligalColumns(temp, 380) == FALSE){
      print("Adding Data")
      allData <- rbind(allData, temp)  
    } else {
      illigalDataSets <- illigalDataSets + 1
      print("NOT adding data")
    }
  }
  
  print(paste("Illigal DataSets:", illigalDataSets))
  
  return(allData)
}