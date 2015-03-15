source('file_locator.r')
source('image_reader.r')
source('knn_runner.r')
source('normalization.r')
source('knn_cross_validation.r')

dpi <- 100
split <- 0.5
k_arr <- c(1)

memberInfo <- getSingleMemberImages('C:\\Users\\Thomas\\Documents\\Subversion\\', 'group6', 'member1');

#Set sigma to 0 to disable
sigma <- 0;

#Get the training data
data <- loadPersonsImageData(memberInfo, sigma, dpi);

#Do the normalization before PCA
data_norm_before <-z_score_normalize(data);
#Perform PCA on the data_norm_before
data_pca_before <- NULL;

#Containing the result from before
data_before <- data_pca_before;

#Perform the PCA on the dat
data_pca_after <- NULL;
#Normalize the data after PCA
data_norm_after <- z_score_normalize(data_pca_after);
#Containing the result for after
data_after <- Normalize;

#Run KNN cross-validation
cross_data_after <- knnCrossValidation(data_after, 10, k_arr, split)
cross_data_before <- knnCrossValidation(data_before, 10, k_arr, split)

#TODO Analyze results (Diagrams?)