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
  
  ##Working directory
  setwd(directory)
  
  z <- NULL
  
  for(i in id){
    
    ##Read files
    if(i<100){
      i <- formatC(i, digits=2, flag="0")
      i<-paste(i, ".csv", sep ="")
    }
    else{
      i<-paste(i, ".csv", sep = "")
    }
    x <- read.csv(i)
    
    y <- x[,pollutant]
    y <- y[!is.na(y)]
    z <- append(z, y)
  }
  mean(z)
  
}