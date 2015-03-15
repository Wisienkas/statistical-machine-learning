# File contains file location functions to find list of files.


# Function Given a basepath "{basepath}" 
# It will find the groupx/memberx/images
# Given a specific dpi {100, 200, 300}

# Return a list of all paths to images
getImagePaths <- function(basePath, dpiArr) {  
  
  group_folders <- list.dirs(path = basePath)    
  imagePaths <- c();
  
  #Run through all the folders
  lapply(X = group_folders, FUN = function(dirPath){  
    
    #The folder must contain member before we want to look in it
    if(length(grep("member", dirPath)) !=  0) {
      
      #All the files in the folder, which are PNG files
      files <- list.files(dirPath, pattern = ".png", full.names = TRUE);
      
      #Check if the file contains the DPI we want
      lapply(X = files, FUN = function(fileName){     
        #See if the file contains the DPI
        
        #NOTE should it really be done in this way?!?!
        lapply(X = dpiArr, FUN = function(dpi){
          if(length(grep(dpi, fileName)) > 0) {                        
            #Add the path to the list
            imagePaths <<- c(imagePaths, fileName);      
          }
        })        
      })        
    }    
  });  
  
  return (imagePaths);
}


# Function given:
#  - Basepath
#  - group
#  - member
#  - dpi {100, 200, 300}
#
# Return a list of the images for 
#   that member in that group at given dpi.


##Testing
dpiArrs <- c(100,200);
patsh <- getImagePaths("E:\\SML_SVN\\trunk\\", dpiArrs);
print(patsh);