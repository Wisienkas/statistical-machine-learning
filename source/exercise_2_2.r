source('file_locator.r')
source('image_reader.r')
source('knn_runner.r')
source('normalization.r')
source('knn_cross_validation.r')
source('pca_optimization.r')



best_cutoff <- 0.99;
best_k <- 10;
runs <- 10;

#Path to images
myData.path <- getSingleMemberImages('C:\\subversion\\', 'group6', 'member2');

#The raw data
myData.data.raw <- loadPersonsImageData(memberInfo = myData.path, sigma = 0, DPI = 100);

myData.data <- streamlineList(myData.data.raw);

#Perform PCA
myData.pca_first.trunc <- pcaTruncate(data = myData.data, cutoff = best_cutoff);

#Perform normalization
myData.pca_first.norm <- z_score_normalize(myData.pca_first.trunc);

#Perform KNN with cross validation
myData.pca_first.knn <- knnCrossValidation(RawTrainData = myData.pca_first.norm, runs = runs, k = best_k);

#Perform normalization
myData.pca_last.norm <- z_score_normalize(myData.data);

#Perform PCA
myData.pca_last.trunc <- pcaTruncate(data = myData.pca_last.norm, cutoff = best_cutoff);

#Run KNN with cross validation
myData.pca_last.knn <- knnCrossValidation(myData.pca_last.trunc, runs = runs, k = best_k)

myData.no_pca_no_norm.knn <- knnCrossValidation(myData.data, runs = runs, k = best_k);

myData.no_norm_but_PCA.knn <- knnCrossValidation(myData.pca_first.trunc, runs = runs, k = best_k);

#Results
print(paste("No norm, PCA - ", "Mean: ", myData.no_norm_but_PCA.knn[[1]], " SD: ", myData.no_norm_but_PCA.knn[[2]], sep = ""));

print(paste("No norm, no PCA - ", "Mean: ", myData.no_pca_no_norm.knn[[1]], " SD: ", myData.no_pca_no_norm.knn[[2]], sep = ""));
print(paste("PCA first - ", "Mean: ", myData.pca_first.knn[[1]], " SD: ", myData.pca_first.knn[[2]], sep = ""));
print(paste("PCA after - ", "Mean: ", myData.pca_last.knn[[1]], " SD: ", myData.pca_last.knn[[2]], sep = ""));
