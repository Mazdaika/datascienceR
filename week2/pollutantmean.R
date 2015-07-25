pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  data <- getdata(id,directory)
  return(mean(data[[pollutant]], na.rm=TRUE))
#  for(i in id) {
#    
#    print(filepath)
#    filepath<-getfile(i, directory)
#    data <- rbind(data,read.csv(filepath))
#  }
  

}

getdata <- function(id, directory){
  data <- data.frame()
  for(i in id) {
    fullfile <- makefullfilename(i)
    pathparts <- c(directory,"\\", fullfile, ".csv")
    filepath <- paste(pathparts, collapse = "")
    data <- rbind(data,read.csv(filepath))
  }
  return(data)
}

makefullfilename <- function(i)
{
  if(i<10){
    fullfile<-c(0,0,i)
  } else {
    if(i<100){
      fullfile <- c(0,i)
    } else {
      fullfile <- i
    }
  }
  return(fullfile)
}
