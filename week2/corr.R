corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  completedata <- complete(directory)
  completedata <- completedata[completedata[[2]]> threshold,]
  corrs <- vector(mode = "numeric")
##  print(completedata[[1]])
  
  for(i in completedata[[1]]){
    tempdata <- getdata(i, directory)
    corrs <- c(corrs, cor(x=tempdata[[2]], y=tempdata[[3]], use="complete.obs"))
  }
  return(corrs)
}