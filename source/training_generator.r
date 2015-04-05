# Given:
# - [trainingData] Raw Training Data
# - [trainingPc] Split (0.9 or 0.5 etc) 
#
# Return Split data and classes
getRandomSplit <- function(trainingData, trainingPc){
  #randomly split data in a balanced maner:
  trainL = list(1:length(trainingData))
  testL = list(1:length(trainingData))
  for(Nr in 0:(length(trainingData)-1))
  {
    amountEachNumber = nrow(trainingData[[Nr+1]]);
    
    set.seed(1) ## make randomness reproducible here
    rand <- sample(amountEachNumber) #generate  a randomly ordered indexing list the size of the datasample
    
    vector = c(1:amountEachNumber);
    for(i in 1:trunc(amountEachNumber*trainingPc))
    {
      vector[i] = 1;
    }
    for(i in trunc(amountEachNumber*trainingPc)+1:amountEachNumber)
    {
      vector[i] = 2;
    }
    splittingIndexer = vector[rand]
    splitData <- split.data.frame(trainingData[[Nr+1]], factor(splittingIndexer))
    
    trainL[[Nr+1]] <- splitData[[1]]
    testL[[Nr+1]]<- splitData[[2]]
  }  
  
  training <- trainL[[1]]
  testing <- testL[[1]]
  trainClass <- rep(0,nrow(trainL[[1]]) )
  testClass <- rep(0,nrow(testL[[1]]) )
  for(i in 2:10)
  {
    training <- rbind(training, trainL[[i]])
    testing <- rbind(testing, testL[[i]])
    
    trainClass <- append(trainClass, rep(i-1,nrow(trainL[[i]]) ) )
    testClass <- append(testClass, rep(i-1,nrow(testL[[i]]) ) )
  }
  trainClassF <- factor(trainClass)
  testClassF <- factor(testClass)
  
  print(paste("Training Rows:", nrow(training) ));
  print(paste("Training classification Length:", length(trainClassF) ));
  print(paste("Testing Rows:", nrow(testing) ));
  print(paste("Training classification Length:", length(testClassF) ));
  
  return(list(training, testing, trainClassF, testClassF))
  
  #non balanced random split
  # retList[[5]][]
  # dataSamples = nrow(retList[[1]]);
  # 
  # set.seed(1) ## make reproducible here, but not if generating many random samples
  # rand <- sample(dataSamples) #generate  a randomly ordered indexing list the size of the datasample
  # print(rand)
  # vector = c(1:dataSamples);
  # for(i in 1:(dataSamples/2))
  # {
  #   vector[i] = 1;
  # }
  # for(i in (dataSamples/2+1):dataSamples)
  # {
  #   vector[i] = 2;
  # }
  # print(vector)
  # splittingIndexer = vector[rand]
  # print(splittingIndexer)
  #   
  #   
  # splitData <- split.data.frame(retList[[1]], factor(splittingIndexer))
  # training = splitData[[1]]
  # testing = splitData[[2]]
  # 
  # splitClass <- split(retList[[3]], factor(splittingIndexer))
  # trainClassF = splitClass[[1]]
  # testClassF = splitClass[[2]]
}

splitBalanced <- function(largeMatrix, classes, split) {
  # Similar digits in row
  len <- length(classes)
  factors <- nlevels(classes)
  
  inStreak <- 0
  firstFactor <- classes[1]
  for(i in 2:len) {
    if(classes[i] != firstFactor) {
      inStreak <- i - 1
      break()
    }
  }
  print(paste("In streak", inStreak))
  print(paste("In factors", factors))
  persons <- len / (factors * inStreak)
  print(paste("Persons: ", persons))
  takePerDigit <- inStreak * split

  index_arr <- c()
  for(f in 1:(factors * persons)) {
    samp <- sample(x = seq(from = (inStreak * (f - 1)) + 1, to = inStreak * f), replace = FALSE, size = takePerDigit)
    index_arr <- c(index_arr, samp)
  }
  
  trainData <- largeMatrix[index_arr, ]
  testData <- largeMatrix[-index_arr, ]
  trainClassF <- classes[index_arr]
  testClassF <- classes[-index_arr]
  return( list(trainData, testData, trainClassF, testClassF) )
}

push <- function(l, x) {
  assign(l, append(eval(as.name(l)), x), envir=parent.frame())
}

classification <- function(numbers, times) {
  result <- c()
  for(n in numbers)
    result <- c(result, rep(x = n, times = times))
  
  return( factor(levels = unique(numbers), x = result) )
}

streamlineList <- function(largeList) {
  print(paste("Length of large List:", length(largeList)))
  arr <- rbind(largeList[[1]])
  for(i in 2:length(largeList)){
    arr <- rbind(arr, largeList[[i]])
  }
  return(arr)
}

getChunkSplit <- function(largeMatrix, classes, split, chunkSize) {
  chunks <- floor(length(classes) / chunkSize)
  
  trainChunks <- floor(split * chunks)
  
  choosenChunks <- sample(x = 1:chunks, size = trainChunks)

  index_arr <- c()
  for(chunkIndex in choosenChunks) {
    index_arr <- c(index_arr, ((chunkIndex - 1) * chunkSize) + 1:chunkSize)
  }
  
  trainData <- largeMatrix[index_arr, ]
  testData <- largeMatrix[-index_arr, ]
  trainClassF <- classes[index_arr]
  testClassF <- classes[-index_arr]
  return( list(trainData, testData, trainClassF, testClassF) )
}

getFolds <- function(data, classes) {
  splits <- nlevels(classes) * 10
  class.len <- length(classes)
  persons <- class.len / 4000
  
  # Define List
  folds <- list()
  # Iterating folds
  for(fold in 1:10) {
      by <- (class.len / 10) / persons
      from <- ((fold - 1) * (by / 10)) + 1
      index.from <- seq(from = from, to = class.len, by = by)
      index.to <- seq(from = from + 39, to = class.len, by = by)
      index.fold <- list(from = index.from, to = index.to)
      folds <- c(folds, list(index.fold))
  }
  return (folds)
}

getIndexFromFolds <- function(from, to) {
  indexes <- c()
  for(i in 1:length(from)) {
    indexes <- c(indexes, from[i]:to[i])
  }
  return (indexes)
}
