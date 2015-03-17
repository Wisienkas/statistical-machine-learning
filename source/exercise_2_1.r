# Libraries Needed
source("file_locator.r")
source("image_reader.r")
source("pca_optimization.r")
source("training_generator.r")
source("knn_cross_validation.r")
library("FactoMineR")

# Read in Data
ressourcePath <- 'C:/Users/wisienkas/workspace/SML-database'

# Get the paths for independent data
myData.path <- getSingleMemberImages(basePath = ressourcePath, 'group6', 'member1');

# Load in Data for both single person and all persons
myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 1, DPI = 100)
allData.data.raw <- loadAllPeople(basePath = ressourcePath)

myData.data <- streamlineList(myData.data.raw)
allData.data <- streamlineList(allData.data.raw)

myData.classF <- classification(numbers = seq(0:9), times = 400)
allData.classF <- rep(x = myData.classF, times = ( dim(allData.data)[1] / dim(myData.data)[1] ) )

allData.data <- allData.data[1:16000,]
allData.classF <- allData.classF[1:16000]

# 2.1.2  Show the eigenvalues, variance and the accumulated variance 
# of the principal components. (In the report 10-20 values 
# equally distributed, should be sufficient to illustrate the tendencies.) 
plot(PCA(myData.data, graph = FALSE)$eig, col = 3)
plot(PCA(allData.data, graph = FALSE)$eig, col = 2)

# 2.1.3  Show the performance of the K-nearest neighbor algorithm when selecting 
# enough principal components to represent 80%, 90%, 95%, 99% of 
# the accumulated variance. For each test vary K.
cutoff_arr <- c(0.8, 0.9, 0.95, 0.99)
k_arr <- c(1, 3, 7, 13)

cv_runs <- 10

result.names <- c("k", "cutoff", "mean", "sd", "times")

result <- matrix(ncol = 5, byrow = TRUE, dimnames = list(c('result'), result.names));
result <- result[-1,];

# colnames(result) <- result.names

for(cutoff in cutoff_arr) {
  myData.trunc <- pcaTruncate(data = myData.data, cutoff = cutoff)
  allData.trunc <- pcaTruncate(data = allData.data, cutoff = cutoff)
  for(k in k_arr) {
    print(paste("Current k:", k))
    print(paste("Current cutoff:", cutoff))
    # Knn Cross validation could be swapped with just regular Knn
    small <- knnCrossValidation(RawTrainData = myData.trunc, runs = cv_runs, k = k)
    small <- list(k = k, cutoff = cutoff, mean = small[[1]], sd = small[[2]], times = small[[3]])
    
    result <- rbind(result, small)
    
    large <- knnCrossValidation(RawTrainData = allData.trunc, runs = cv_runs, k = k)
    large <- list(k = k, cutoff = cutoff, mean = large[[1]], sd = large[[2]], times = large[[3]])
    
    result <- rbind(result, large)
  }
}

## Plot for data on this form
## > df
##   l.k  l.times l.correctness
## 1   1 3.967227         0.811
## 2   5 3.729213         0.824
## 3  10 3.482199         0.823
##
## ggplot(data = df, aes(x = l.k, y = value, color = variable)) + geom_line(aes(y = l.times, col = "times")) + geom_line(aes(y = l.correctness, col = "correctness"))

### VISUALIZING DATA

## Did something to mean out times, should be done too.

result.allData <- result[-seq(from = 1, to = nrow(result) - 1, by = 2),]
result.myData <- result[-seq(from = 1, to = nrow(result) - 1, by = 2),]

printG <- function(aFrame, x_name, y_name) {
  ## aFrame need to be visible Globally
  aFrame <<- aFrame
  g <- ggplot(aFrame, aes(aFrame[,1]))
  # Adding lines
  g <- g + geom_line(aes(y = aFrame[,2]), colour = "red")
  g <- g + geom_line(aes(y = aFrame[,3]), colour = "green")
  g <- g + geom_line(aes(y = aFrame[,4]), colour = "yellow")
  g <- g + geom_line(aes(y = aFrame[,5]), colour = "blue")
  g <- g + ylab(y_name)
  g <- g + xlab(x_name)
  # Printing
  return(g)
}

getFrame <- function(dim1, dim2, data) {
  # Creating the dataframe
  df <- data.frame(unique(unlist(unname(data[, dim1]))),
              unlist(unname(data[1:4, dim2])),
              unlist(unname(data[5:8, dim2])),
              unlist(unname(data[9:12, dim2])),
              unlist(unname(data[13:16, dim2])))
  return( df )
}

getFrame2 <- function(dim1, dim2, data) {
  # Creating the dataframe
  df <- data.frame(unique(unlist(unname(data[, dim1]))),
                   unlist(unname(data[seq(from = 1, to = 13, by = 4), dim2])),
                   unlist(unname(data[seq(from = 2, to = 14, by = 4), dim2])),
                   unlist(unname(data[seq(from = 3, to = 15, by = 4), dim2])),
                   unlist(unname(data[seq(from = 4, to = 16, by = 4), dim2])))
  return( df )
}

printG(aFrame = getFrame(1, 3,result.allData), "K", "Mean - Correctness")
printG(aFrame = getFrame(1, 3,result.myData), "K", "Mean - Correctness")

printG(aFrame = getFrame(1, 4,result.allData), "K", "SD - Correctness")
printG(aFrame = getFrame(1, 4,result.myData), "K", "SD - Correctness")

printG(aFrame = getFrame(1, 5,result.allData), "K", "Mean - Time")
printG(aFrame = getFrame(1, 5,result.myData), "K", "Mean - Time")

printG(aFrame = getFrame2(2, 3,result.allData), "cutoff", "Mean - Correctness")
printG(aFrame = getFrame2(2, 3,result.myData), "cutoff", "Mean - Correctness")

printG(aFrame = getFrame2(2, 4,result.allData), "cutoff", "SD - Correctness")
printG(aFrame = getFrame2(2, 4,result.myData), "cutoff", "SD - Correctness")

printG(aFrame = getFrame2(2, 5,result.allData), "cutoff", "Mean - Time")
printG(aFrame = getFrame2(2, 5,result.myData), "cutoff", "Mean - Time")
