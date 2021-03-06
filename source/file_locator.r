# File contains file location functions to find list of files.

# Function Given a basepath "{basepath}" 
# It will find the groupx/memberx/images
# returning a matrix containing all information about each member
# Columns in matrix:
# - grup_name   : text
# - member_name : text
# - res_100     : list
# - res_200     : list 
# - res_300     : list
# - corner_file : text

# Return a list of all paths to images
getAllMemberImages <- function(basePath) {  
  group_folders <- list.dirs(path = basePath)    
  members <- matrix(ncol = 6, byrow = TRUE, dimnames = list(c('Member'), c('group_name', 'member_name', 'res_100', 'res_200', 'res_300', 'corner_file')));
  members <- members[-1,];
  
  #Run through all the folders
  lapply(X = group_folders, FUN = function(dirPath){
    
    #The folder must contain member before we want to look in it
    if(length(grep("member", dirPath)) <  1) {
      return();
    }
    
    #Get group and member names
    group_name <- substr(dirPath, nchar(dirPath) - 13, nchar(dirPath) - 13 + 5);
    member_name <- substr(dirPath, nchar(dirPath) - 6, nchar(dirPath));
    
    #Get images for that user
    member <- getSingleMemberImages(basePath, group_name, member_name);
    #print(member);
    
    if(length(member) > 0 && !is.null(member)) {
      #Add row to matrix
      members <<- rbind(members, member);
    }
  });  
  
  return (members);
}

# Function given:
#  - Basepath
#  - group
#  - member
#
# Returns a list containing the following:
#   - grup_name   : text
#   - member_name : text
#   - res_100     : list 
#   - res_200     : list
#   - res_300     : list
#   - corner_file : text

getSingleMemberImages <- function(basePath, group_name, member_name) {
  dirPath <- paste(basePath, "/", group_name, "/", member_name, sep = "");
  #All the files in the folder, which are PNG files
  files <- list.files(dirPath, pattern = ".png", full.names = TRUE);
  
  #The member object
  corner_file <- paste(dirPath, '/Corners.txt', sep = "");
  res_100_files <- NULL;
  res_200_files <- NULL;
  res_300_files <- NULL;
  
  #Check if the file contains the DPI we want
  lapply(X = files, FUN = function(fileName){     
    #Add correct files to lists
    if(length(grep('100', fileName)) > 0) {
      res_100_files <<- c(res_100_files, fileName);
    }
    else if(length(grep('200', fileName)) > 0) {
      res_200_files <<- c(res_200_files, fileName);
    }
    else if(length(grep('300', fileName)) > 0) {
      res_300_files <<- c(res_300_files, fileName);
    }
  });
  
  #No image files found, do not add
  if(is.null(res_100_files) && is.null(res_200_files) && is.null(res_300_files)) {
    return();
  }
  #if(length(res_100_files) == 0 && length(res_200_files) == 0 && length(res_300_files) == 0) {
    #return();
  #}
  
  #Prepare information to be added to the matrix
  member <- list(group_name, member_name, res_100_files, res_200_files, res_300_files, corner_file);
  return (member);
};

getCornerPath <- function(member, row)
{
  if(is.null(dim(member))) {
    return (member[6]);
  }
  else{
    return (member[row, 6]);
  }
}

getDpiImage <- function(member, row, dpi) {
  if(is.null(dim(member))) {
    if(dpi == 100) {
      return (member[3]);
    }
    if(dpi == 200){
      return (member[4]);
    }
    if(dpi == 300){
      return (member[5]);
    }
  }
  else {
    if(dpi == 100) {
      return (member[row,3]);
    }
    if(dpi == 200){
      return (member[row,4]);
    }
    if(dpi == 300){
      return (member[row,5]);
    }
  }
}

##Testing
#member <- getSingleMemberImages('C:\\subversion\\', 'group6', 'member2');
#print(member);
#mInfo <- getAllMemberImages("C:\\subversion\\");
#for(row in 1:nrow(mInfo))
#{
#print(mInfo[row,]);
#}