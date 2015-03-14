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
  
  print(nrow(training) );
  print(length(trainClassF) );
  print(nrow(testing) );
  print(length(testClassF) );
  
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