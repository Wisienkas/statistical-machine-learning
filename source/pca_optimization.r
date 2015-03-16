source("training_generator.r")

# Read Material http://stackoverflow.com/questions/3905958/how-to-reduce-the-number-of-attributes-to-20-using-prcomp-in-r
#
# Found solution here
# http://stats.stackexchange.com/questions/57467/how-to-reconstruct-original-data-using-a-small-number-of-principal-components


pcaTruncate <- function(data, cutoff) {
  # Makes the data into 1 2d array instead of a list 
  myData <- data
  
  #levels <- seq(0:9)
  
  # Add classification for data
  #myData.labels <- classification(rep(x = levels, times = length(rawData) / length(levels)), 400)
  
  # Perform PCA on data
  myData.pca <- prcomp(myData, center = TRUE)
  
  myData.pca.cumulative <- cumsum(myData.pca$sdev ^ 2 / sum(myData.pca$sdev ^ 2))
  
  cutoffIndex <- sum(myData.pca.cumulative < cutoff) + 1
  
  myData.trunc <- predict(myData.pca)[, 1:cutoffIndex]
  
  return (myData.trunc)
  # prcomp$sdev is Standard Deviation of principal components eigenvalue.
  # Taking the power of 2 to get eig
  #myData.pca.eig <- myData.pca$sdev ^ 2
  
  #totalVarMyData <- sum(myData.pca.eig)
  #myData.pca.var <- myData.pca.eig / totalVarMyData
  
  #return(myData)
}



