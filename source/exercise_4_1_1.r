library(MASS)
library(tree)
library("C50")

source("file_locator.r")
source("image_reader.r")
source("training_generator.r")
source("pca_optimization.r")

# Loading the data
# Nikolaj PATHs: 
# Home pc:  C:/Users/wisienkas/workspace/SML-database
# Laptop :  /home/wisienkas/workspace/school/6sem/sml/exercise/svn/trunk
#
# Thomas PATHs:
#
basePath <- "C:/Users/wisienkas/workspace/SML-database"
myData.rawData.path <- getSingleMemberImages(basePath = basePath, group_name = "group6", member_name = "member1")
myData.rawData <- loadPersonsImageData(memberInfo = myData.rawData.path, sigma = 1, DPI = 100)

myData.data <- streamlineList(largeList = myData.rawData)
myData.class <- classification(numbers = 0:9, times = nrow(myData.data) / 10)

myData.pca50 <- pcaGetComponets(myData.data, 50)
allData.pca50 <- pcaGetComponets(allData.data, 50)

splitMatrix <- function(matrix, split) {
  splits <- split * nrow(matrix)
  trainRows <- sample(1:nrow(matrix), size = splits, replace = FALSE)
  
  trainData <- matrix[trainRows, ];
  testData <- matrix[-trainRows, ];
  
  return(list("train" = trainData, "test" = testData))
}

getMaxInRow <- function(data) {
  result <- c()
  for(i in 1:nrow(data)) {
    result <- c(result, max.col(data, ties.method = "first"))
  }
  return (result)
}

entropy <- function(p, n) {
  a <- -p / (n + p) * log2(p / (n + p))
  b <- -n / (n + p) * log2(n / (n + p))
  return (a + b)
}

IG <- function(before, after) {
  return (before - after)
}

entropy.data <- function(data, class, threshold) {
  if(is.vector(data)) {
    data <- split(data, class)
    ent <- c()
    for(cl in data) {
      cl <- ifelse(cl < threshold, TRUE, FALSE);
      p <- sum(cl)
      n <- length(cl) - p
      ent <- c(ent, entropy(p, n))
    }
    return(ent)
  } else {
    ent <- data.frame();
    for(col in 1:ncol(data)) {
      ent <- rbind(ent, entropy.data(data[, col], class, threshold))
    }
    colnames(ent) <- 1:ncol(ent)
    return(ent)
  }
}

myData.pca50 <- cbind(myData.pca50, myData.class)

myData.entropy <- list()
for(t in seq(from = min(myData.pca50[, 1:50]), to = max(myData.pca50[, 1:50]), by = 0.05)) {
  myData.entropy[[paste("t", t, sep = "")]] <- entropy.data(myData.pca50[, 1:50], myData.pca50[, 51], t)
}

ka <- c()
for(cl in myData.entropy) {
  ka <- c(ka, cl[1, 1])
}
ka <- 1 - ka[!is.na(ka)]
plot(1:length(ka), 1 - ka)

myData.split <- splitMatrix(myData.pca50, 0.9)

myData.train <- as.data.frame(myData.split[["train"]])
myData.test <- as.data.frame(myData.split[["test"]])

colnames(myData.train) <- paste("col", 1:51, sep = "")
colnames(myData.test) <- paste("col", 1:51, sep = "")

m <- C5.0(x = myData.train[, 1:50], y = factor(myData.train[, 51]), trials = 1, costs = NULL)
p <- predict(m, myData.test[, 1:50])

mean(p == myData.test[, 51])

library(rJava)
library(FSelector)
library(lattice)
weight <- information.gain(myData.class ~ . , data = as.data.frame(myData.pca50))
barplot(weight[,1], names.arg = rownames(weight), border = TRUE, xlab = "Principle Components", ylab = "Information Gain", main = "Information Gain for the Principle Components")

## Initial tree

tree_model <- tree(factor(col51) ~ . , data = as.data.frame(myData.train))

summary(tree_model)
plot(tree_model)
text(tree_model, pretty = 0)

# Check tree performance

tree_predict <- predict(tree_model, as.data.frame(myData.test))
tree_predict <- getMaxInRow(tree_predict)

correctness <- mean(tree_predict == myData.test[, 51])


pca50 <- allData.pca50
class <- allData.class

ent <- list()
ent_bf <- sum(rep(-0.1*log2(0.1), times = 10))
for(pc in 1:50) {
  col <- pca50[, pc]
  prob <- c()
  for(t in seq(from = (min(col) + 0.01), to = (max(col) - 0.01), by = 0.01)) {
    candidates <- col < t
    splitted <- split(class, candidates)
    prob <- c(prob, myEntro(splitted))
  }
  ent[[pc]] <- list("ent" = prob, "seq" = seq(from = (min(col) + 0.01), to = (max(col) - 0.01), by = 0.01))
  indexes <- which(!is.na(ent[[pc]]$ent))
  ent[[pc]]$ent <- ent[[pc]]$ent[indexes]
  ent[[pc]]$seq <- ent[[pc]]$seq[indexes]
}

minY <- 999
maxY <- -999
minX <- 999
maxX <- -999

for(pc in ent) {
  if(max(pc$ent) > maxY) maxY <- max(pc$ent)
  if(min(pc$ent) < minY) minY <- min(pc$ent)
  if(max(pc$seq) > maxX) maxX <- max(pc$seq)
  if(min(pc$seq) < minX) minX <- min(pc$seq)
}

count <- 1
for(pc in ent) {
  if(count == 1)
  {
    plot(pc$seq, y = pc$ent, col = count, type = "l", xlim = c(minX, maxX), 
         ylim = c(minY, maxY), ylab = 'Entropy', xlab = 'Threshold', 
         main = "Entropy values for the first 50 PCA")
  }
  else
  {
    lines(pc$seq, y = pc$ent, col = count)
  }
  count <- count + 1
}
grid()
legend("bottomleft", fill = 1:8, legend = c(1:8), title = "first 8 PCA's")

myEntro <- function(data) {
  ps <- c()
  total <- 0
  for(branch in data) {
   total <- total + length(branch) 
  }
  for(branch in data) {
    count <- c()
    for(level in levels(branch)) {
      count <- c(count, sum(branch == level))
    }
    # total amount in branch
    q_t <- sum(count)
    # Probability in branch
    q_p <- count / q_t
    # sum of entropy
    q_s <- sum(-q_p * log2(q_p))
    # weight for branch
    q_w <- count / total
    ps <- c(ps, q_w * q_s)
  }
  return (sum(ps))
}

# Adding Information Gain
for(pc in 1:length(ent)) {
    ent[[pc]]$ig <- ent_bf - ent[[pc]]$ent
}

minY <- 999
maxY <- -999
minX <- 999
maxX <- -999

for(pc in ent) {
  if(max(pc$ig) > maxY) maxY <- max(pc$ig)
  if(min(pc$ig) < minY) minY <- min(pc$ig)
  if(max(pc$seq) > maxX) maxX <- max(pc$seq)
  if(min(pc$seq) < minX) minX <- min(pc$seq)
}

count <- 1
for(pc in ent) {
  if(count == 1)
  {
    plot(pc$seq, y = pc$ig, col = count, type = "l", xlim = c(minX, maxX), 
         ylim = c(minY, maxY), ylab = 'Information Gain', xlab = 'Threshold', 
         main = "Information Gain for the first 50 PCA")
  }
  else
  {
    lines(pc$seq, y = pc$ig, col = count)
  }
  count <- count + 1
}
grid()
legend("bottomleft", fill = 1:8, legend = c(1:8), title = "first 8 PCA's")

#### EASY EXAMPLE !
q <- c(3,5,10,7,1,1,8,3)
q_t <- length(q)

q_p <- q / q_t

q_s <- -q_p * log2(q_p)
sum(q_s)


