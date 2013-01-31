agecount <- function(age = NULL) {
  ## Check that "age" is non-NULL; else throw error
  
  if (is.null(age)) stop("error!!! age must not NULL")
  
  ## Read "homicides.txt" data file
  homicide <- readLines("homicides.txt")
  
  ## Extract ages of victims; ignore records where no age is
  ## given
  ages <- regexpr(paste0("<dd>(.*?) ", as.character(age), " years old</dd>"), homicides)
  
  ## Return integer containing count of homicides for that age
  cnt <- regmatches(homicides, ages)
  
  length(cnt)
}