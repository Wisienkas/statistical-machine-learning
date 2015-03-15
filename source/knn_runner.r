source('training_generator.r')

# Given:
# - [RawTrainData] -> Raw training data
# - [k_arr] -> Array of k values to test
# - [dpi] -> DPI of images
# - [text_result] -> Previous results or string. 
#
# Return [text_result] appended with result
knnRunner <- function(RawTrainData, k_arr, dpi, text_result)
{
  # Exercise 1.5.1
  #randomly split data in equal sizes:
  retList = getRandomSplit(RawTrainData, 0.5)
  training = retList[[1]]
  testing = retList[[2]]
  trainClassF = retList[[3]]
  testClassF = retList[[4]]
  
  # Knn for different k's
  for(j in 1:length(k_arr))
  {
    start_time <- as.numeric(Sys.time())
    knn_result <- knn(train = training, test = testing, cl = trainClassF, k_arr[[j]])
    correctness <- mean(testClassF == knn_result)
    stop_time <- as.numeric(Sys.time()) - start_time
    # Adds result to a vector
    text_result <- rbind(text_result, paste("DPI: ", dpi, " K: ", k_arr[[j]], " Time", stop_time, " Correctness: ", correctness))
  }
  return(text_result)
}