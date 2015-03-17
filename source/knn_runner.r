source('training_generator.r')

# Given:
# - [RawTrainData] -> Raw training data
# - [k_arr] -> Array of k values to test
# - [dpi] -> DPI of images
# - [text_result] -> Previous results or string. 
#
# Return [text_result] appended with result
knnRunner <- function(RawTrainData, k_arr, dpi, text_result, split)
{
  #randomly split data in equal sizes:
  retList = getRandomSplit(RawTrainData, split)
  training = retList[[1]]
  testing = retList[[2]]
  trainClassF = retList[[3]]
  testClassF = retList[[4]]
  
  training.pca <- pca_object(training, 0.9)
  testing.pca <- pca_object(testing, 0.9)
  
  correctness <- c()
  time <- c()
  
  # Knn for different k's
  for(j in 1:length(k_arr))
  {
    start_time <- as.numeric(Sys.time())
    knn_result <- knn(train = training.pca, test = testing.pca, cl = trainClassF, k_arr[[j]])
    correctness <- c(correctness, mean(testClassF == knn_result))
    stop_time <- as.numeric(Sys.time()) - start_time
    time <- c(time, stop_time)
    # Adds result to a vector
    #text_result <- rbind(text_result, paste("DPI: ", dpi, " K: ", k_arr[[j]], " Time", stop_time, " Correctness: ", correctness))
  }
  return(text_result)
}

knn.run <- function(largeMatrix, classes, k_arr, split, pca = FALSE) {
  
  knn.data <- splitBalanced(largeMatrix = largeMatrix, classes = classes, split = split)
  #knn.data <- getChunkSplit(largeMatrix = largeMatrix, classes = classes, split = split, chunkSize = 4000)
  knn.train <- knn.data[[1]]
  knn.test <- knn.data[[2]]
  knn.trainClassF <- knn.data[[3]]
  knn.testClassF <- knn.data[[4]]
  
  result.names <- c("k", "split", "correctness", "times")
  
  result <- matrix(ncol = 4, byrow = TRUE, dimnames = list(c('result'), result.names));
  result <- result[-1,];
  
  for(k in k_arr) {
    time <- as.numeric(Sys.time())
    knn_result <- knn(train = knn.train, test = knn.test, cl = knn.trainClassF, k = k)
    correctness <- mean(knn.testClassF == knn_result)
    time <- as.numeric(Sys.time()) - time
    res <- list(k = k, split = split, correctness = correctness, times = time)
    result <- rbind(result, res)
  }
  
  return( result )
}

knn.run_split <- function(largeMatrix, classes, k_arr, split, chunkSize) {
  knn.data <- getChunkSplit(largeMatrix = largeMatrix, classes = classes, split = split, chunkSize = chunkSize)
  knn.train <- knn.data[[1]]
  knn.test <- knn.data[[2]]
  knn.trainClassF <- knn.data[[3]]
  knn.testClassF <- knn.data[[4]]
  
  result.names <- c("k", "split", "correctness", "times")
  
  result <- matrix(ncol = 4, byrow = TRUE, dimnames = list(c('result'), result.names));
  result <- result[-1,];
  
  for(k in k_arr) {
    time <- as.numeric(Sys.time())
    knn_result <- knn(train = knn.train, test = knn.test, cl = knn.trainClassF, k = k)
    correctness <- mean(knn.testClassF == knn_result)
    time <- as.numeric(Sys.time()) - time
    res <- list(k = k, split = split, correctness = correctness, times = time)
    result <- rbind(result, res)
  }
  
  return( result )
}