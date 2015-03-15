source('file_locator.r')
source('image_reader.r')
source('knn_runner.r')

dpi <- c(100)
k_arr <- c(1, 5, 10, 50)

text <- c()
memberInfo <- getSingleMemberImages('C:\\Users\\Thomas\\Documents\\Subversion\\', 'group6', 'member1');

#Set sigma to 0 to disable
sigma <- 0;

for(i in 1:length(dpi)) { 
  RawTrainData <- loadPersonsImageData(memberInfo, sigma, dpi[[i]]);
  text <- knnRunner(RawTrainData, k_arr, dpi[[i]], text, 0.5)
}

# print all 50/50 results 
for(t in 1:length(text)) {
  print(text[[t]])
}