source("training_generator.r")

pca_object <- function(rawData) {
  # Makes the data into 1 2d array instead of a list 
  myData <- streamlineList(rawData);
  
  # Add classification for data
  myData.labels <- classification(numbers = 0:9, times = 400)
  
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
  return(arr)
}
