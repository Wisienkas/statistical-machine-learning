# Given:
# - [RawTrainData] -> Raw image data in grayscale
# - [runs] -> How many runs for crossvalidation
# - [k] -> neighboors to look for in knn function
#
# Return Text result of knn - Crossvalidation
knnCrossValidation <- function(RawTrainData, runs, k, split) {
  #randomly split data in 0.9 / 0.1:
  knn_obs_data <- c();
  
  retList = getRandomSplit(RawTrainData, split)
  training = retList[[1]]
  testing = retList[[2]]
  trainClassF = retList[[3]]
  testClassF = retList[[4]]
  
  # Initialize variable: sum
  sum <- 0
  
  for(l in 1:runs) 
  {  
    # combining training and testing
    train <- rbind(training, testing)
    # Combines the train and test class, so they keep their data form.
    cla <- factor(c(trainClassF, testClassF), levels = 1:nlevels(trainClassF), labels = levels(trainClassF))
    # Running knn.cv (crossvalidation)
    knn_res <- knn.cv(train = train, cl = cla, k = k, prob = TRUE)
    # Makes a crosstable based on the validation of the data
    ct <- CrossTable(cla, knn_res, prop.r = TRUE)
    for(point in 1:10)
    {
      sum <- sum + ct$prop.row[point, point]
      knn_obs_data <- rbind(knn_obs_data, ct$prop.row[point, point])
    }
  }
  # mean is the total probability added together divided by 
  mean <- sum / (10 * runs)
  
  # Is read from the inner paranthesis
  # 1. The function lapply run the function "FUN" defined below on 
  # every entry in the list knn_obs_data. 
  # 
  # 2. lapply then return the list of results as a list, 
  # 
  # 3. the list is then converted to "numeric" because "sum" is very picky 
  # 
  # 4. the sum is done for the list
  #
  # 5. the sum is divided. (At this stage we have the "variance")
  # 
  # 6. Square the variance which yields the standard deviation
  sd <- sqrt(sum(as.numeric((lapply(X = knn_obs_data, FUN = function(X){
    diff <- mean - X
    return(diff ^ 2)
  })))) / length(knn_obs_data))
  return(c(mean, sd))
}