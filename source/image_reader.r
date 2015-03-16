library("png")
library("EBImage")
library("class")
library("gmodels")
source('file_locator.r')


# Given:
#
# - List of Path to images
# - Sigma value (default 1)
# 
# returns list of image (raw pixel values)
loadPersonsImageData <- function(memberInfo, sigma, DPI){
  #load the scaned images
  
  paths <- getDpiImage(memberInfo, 1, DPI)[[1]]
  print(paths)
  
  ciffers <- lapply(X = paths, FUN = readPNG);
  #load the corner values
  corners <- read.csv(getCornerPath(memberInfo, 1)[[1]])
  print("recalculating corners for current dpi")
  corners <- trunc(corners*DPI/300)
  print(corners)
  
  #define lists to be used
  gray <- list(1:5)
  smoothed <- list(1:5)
  prepared <- list(1:5)
  
  
  #convert the images to gray scale.
  for(i in 1:5)
  {
    r <-ciffers[[i]][,,1]
    g <-ciffers[[i]][,,2]
    b <-ciffers[[i]][,,3]
    gray[[i]] <- (r+g+b)/3
  }
  
  #smooth images
  for(i in 1:5)
  {
    smoothed[[i]] <- smoothImage(gray[[i]], sigma = sigma)
  }
  
  #generate image that is prepared for learning and visualization
  for(i in 1:5)
  {
    prepared[[i]] <- smoothed[[i]]
  }
  
  #extract individual ciffers
  xStep <- (corners[1,7]-corners[1,1])/20;
  yStep <- (corners[1,8]-corners[1,2])/20;
  xStepT <- trunc(xStep)
  yStepT <- trunc(yStep)
  
  tempM <- matrix(,20*20,(yStepT-2)*(xStepT-2))
  trainingDigit <- list(1:10);
  
  for(pages in 1:5)
  {
    for(box in 1:2)
    {
      #     trainingDigit[[(pages-1)*2 + box]] <- matrix(,20*20,(yStepT-2)*(xStepT-2))) 
      for(cifX in 1:20)
      {
        aXbase <- corners[(pages-1)*2 + box,1] + xStep*(cifX-1)
        for(cifY in 1:20)
        {
          aYbase <- corners[(pages-1)*2 + box,2] + yStep*(cifY-1)
          
          for(px in 1:xStepT-2)
          {
            for(py in 1:yStepT-2)
            {
              tempM[(cifY-1)*20 + cifX, (px-1)*(yStepT-2) + py] <- prepared[[pages]][aYbase+py+1,aXbase+px+1]
            }
          }
        }
      }
      trainingDigit[[(pages-1)*2 + box]] <- tempM
    }
  }
  
  #use the generated training data to apply learning
  #  No reason to printing data every time. 
  # print( trainingDigit[[1]] )
  
  return(trainingDigit)
}


#inspiration for smoothing
smoothImage <- function(grayImg, sigma){
  #two ways of specifying kernel:
  # kernel <- matrix( 
  #           c(1, 1, 1, 
  #             1, 1, 1, 
  #             1, 1, 1), # the data elements 
  #           3,              # number of rows 
  #           3)
  # kernel <- kernel/9
  # kernel
  
  kernel <- matrix( 
    1, # the data elements 
    3,# number of rows 
    3)
  kernel <- kernel/9
  # print(kernel)
  
  #using r library for smoothing
  smoothed <- NULL;
  if(sigma == 0) {
    smoothed <- filter2(grayImg, kernel);
  }
  else {
    smoothed <- filter2(gblur(grayImg, sigma = sigma), kernel)
  }
  
  #simple implementation of average filter:
  # imgWidth <- length(gray[1,])
  # imgHeight <- length(gray[,1])
  # kernelSize <- 1
  # for(px in 1:imgWidth)
  # {
  #   for(py in 1:imgHeight)
  #   {
  #     baseX <- px - kernelSize
  #     endX <- px + kernelSize
  #     if(baseX < 1){baseX<-1}
  #     if(endX > imgWidth){endX<-imgWidth}
  #     
  #     baseY <- py - kernelSize
  #     endY <- py + kernelSize
  #     if(baseY < 1){baseY<-1}
  #     if(endY > imgHeight){endY<-imgHeight}
  #     
  #     
  #     value <- 0
  #     for(pkx in baseX:endX)
  #     {
  #       for(pky in baseY:endY)
  #       {
  #         value <- value+gray[pky,pkx]
  #       }
  #     }
  #     kernelValues <- (endY-baseY+1)*(endX-baseX+1)    
  #     value <- value/kernelValues
  #     
  #     smoothed[py,px] <- value
  #   }
  # }
  return(smoothed)
}
# =============
# Testing
# =============
#images <- c("C:\\Users\\wisienkas\\workspace\\SML-database\\group6\\member1\\Ciphers100-0.png",
#            "C:\\Users\\wisienkas\\workspace\\SML-database\\group6\\member1\\Ciphers100-1.png",
#            "C:\\Users\\wisienkas\\workspace\\SML-database\\group6\\member1\\Ciphers100-2.png",
#            "C:\\Users\\wisienkas\\workspace\\SML-database\\group6\\member1\\Ciphers100-3.png",
#            "C:\\Users\\wisienkas\\workspace\\SML-database\\group6\\member1\\Ciphers100-4.png");

#rawData <- loadPersonsImageData(images, 1, 100, "C:\\Users\\wisienkas\\workspace\\SML-database\\group6\\member1\\Corners.txt")

haveIlligalColumns <- function(data, requiredCols) {
  for(el in data){
    if(ncol(el) != requiredCols) {
      print(paste("Illigal amount of Columns! Had:", ncol(el)))
      return(TRUE)      
    }
  }
  return(FALSE)
}

loadAllPeople <- function(basePath) {
  
  allPaths <- getAllMemberImages(basePath)
  
  illigalDataSets <- 0
  
  allData <- NULL
  # Index [1, ] is NA all over
  for(personNumber in 1:length(allPaths[, 1])) {
    temp <- loadPersonsImageData(allPaths[personNumber, ], 1, 100)
    # Handle if people doesn't have the right amount of columns as 
    # it fucks up things.
    if(haveIlligalColumns(temp, 380) == FALSE){
      print("Adding Data")
      allData <- rbind(allData, temp)  
    } else {
      illigalDataSets <- illigalDataSets + 1
      print("NOT adding data")
    }
  }
  
  print(paste("Illigal DataSets:", illigalDataSets))
  
  return(allData)
}