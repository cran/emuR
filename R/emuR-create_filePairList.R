## Create a file-pair-list
##
## Recursivly searches through a root directory and matches the
## basenames of files that have the extentions provided.
##
## 
## @param ext1Path2rootDir path to root directory of first file extention 
## @param ext2Path2rootDir path to root directory of second file 
## extention (CAUTION: think of DB size and search space!) 
## @param ext1 first extention to look for. This extention is considered 
## the primary extention. 
## This means that this extentions genarates the basename list that the 
## second extentions list is matched against.
## @param ext2 second extention to pair base names of first extention with
## @import tools
##
create_filePairList <- function(ext1Path2rootDir, ext2Path2rootDir, ext1, ext2){
  # normalize paths
  ext1Path2rootDir = suppressWarnings(normalizePath(ext1Path2rootDir))
  ext2Path2rootDir = suppressWarnings(normalizePath(ext2Path2rootDir))
  
  # ext1Path2rootDir is valid path
  if(!dir.exists(ext1Path2rootDir)){
    stop(paste0('ext1Path2rootDir does not exist: ',
                ext1Path2rootDir))
  }
  
  # ext2Path2rootDir is valid path
  if(!dir.exists(ext2Path2rootDir)){
    stop(paste0('ext2Path2rootDir does not exist: ',
                ext2Path2rootDir))
  }
  
  
  # get all ext1 file paths
  allExt1FilePaths = list.files(ext1Path2rootDir, 
                                pattern = paste(ext1, "$", sep = ""), 
                                recursive = TRUE,
                                full.names = TRUE)
  
  # get all ext2 file paths
  allExt2FilePaths = list.files(ext2Path2rootDir, 
                                pattern = paste(ext2, "$", sep = ""), 
                                recursive = TRUE,
                                full.names = TRUE)
  
  # check more ext1 found than ext2
  if(length(allExt1FilePaths) > length(allExt2FilePaths)){
    stop("Found less files with '", 
         ext2, "' extension than files with '", 
         ext1, "' extension in ", 
         ext1Path2rootDir, " and ", 
         ext2Path2rootDir, " including their sub-directories!")
  }
  
  # extract base names
  allExt1FilePathsBNs = basename(sub(pattern = "(.*)\\..*$", replacement = "\\1", allExt1FilePaths))
  allExt2FilePathsBNs = basename(sub(pattern = "(.*)\\..*$", replacement = "\\1", allExt2FilePaths))
  
  equalToExt1FilePathsBNs = allExt1FilePathsBNs[allExt2FilePathsBNs %in% allExt1FilePathsBNs]
  foundExt2FilePaths = allExt2FilePaths[allExt2FilePathsBNs %in% allExt1FilePathsBNs]
  
  
  # check if found all allExt2FilePathsBNs in allExt1FilePathsBNs
  if(length(allExt1FilePathsBNs) != length(equalToExt1FilePathsBNs)){
    stop("Not all '", 
         ext2, 
         "' files found for '", 
         ext1, 
         "' files found in ", 
         ext1Path2rootDir, 
         " and ", 
         ext2Path2rootDir, 
         " including their sub-directories!") 
  }
  
  
  # check they are empty
  if(length(allExt1FilePathsBNs)==0 || length(allExt2FilePathsBNs) == 0){
    stop('Both colomns in file pair list are empty! This means that no files where found...') 
  }
  
  # cbind filePairList
  fpl = cbind(allExt1FilePaths, foundExt2FilePaths)
  
  colnames(fpl) = c('ext1FilePaths', 'ext2FilePaths')
  
  return(fpl)
}

# FOR DEVELOPMENT
# library('testthat')
# test_file('tests/testthat/test_create.filePairList.R')
