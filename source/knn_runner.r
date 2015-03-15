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