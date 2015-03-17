# Libraries
source("training_generator.r")

# Given:
# - [RawTrainData] -> Raw image data in grayscale
# - [runs] -> How many runs for crossvalidation
# - [k] -> neighboors to look for in knn function
#
# Return Text result of knn - Crossvalidation
knnCrossValidation <- function(RawTrainData, runs, k) {
  
  #randomly split data in 0.9 / 0.1:
  knn_obs_data <- c();
  
  if(class(RawTrainData) == "list") {
    RawTrainData <- streamlineList(largeList = RawTrainData)
  }
  
  train <- RawTrainData
  cla <- classification(numbers = seq(1:10), times = 400)
  cla <- rep(x = cla, times = ( dim(RawTrainData)[1] / length(cla) ) )
  if(length(cla) != dim(RawTrainData)[1]) {
    print("Failed Class")
    stop(cla, dim(RawTrainData), domain = "Classification Failed")
  }
  # Initialize variable: sum
  sum <- 0
  
  time <- c()
  
  for(l in 1:runs) 
  {  
    start_time <- as.numeric(Sys.time())
    # Running knn.cv (crossvalidation)
    knn_res <- knn.cv(train = train, cl = cla, k = k, prob = TRUE)
    
    len <- length(knn_res)
    #if(sum(knn_res[1:200] == 2) >= 150)
    #  knn_res <- correctBug(knn_res)
    
    if(len != length(knn_res))  {
      print("Failed bug")
      print(knn_res)
      print(len, length(knn_res))
      stop(len, knn_res, domain ="Bug fix failed")
    }
    # Makes a crosstable based on the validation of the data
    ct <- CrossTable(cla, knn_res, prop.r = TRUE)
    for(point in 1:10)
    {
      sum <- sum + ct$prop.row[point, point]
      knn_obs_data <- rbind(knn_obs_data, ct$prop.row[point, point])
    }
    stop_time <- as.numeric(Sys.time()) - start_time
    time <- c(time, stop_time)
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
  
  return(list(mean, sd, time))
}

correctBug <- function(knn_result) {
  for(index in 1:length(knn_result)) {
    if(knn_result[index] == 2) {
      knn_result[index] <- 1
    } else if(knn_result[index] == 1) {
      knn_result[index] <- 2
    }
  }
  return(knn_result)
}