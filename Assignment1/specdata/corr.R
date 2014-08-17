corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  p <- getwd()
  setwd(directory)
  
  r <- NULL
  
  id <- 1:332
  
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
    
    y <- x[complete.cases(x),]
    
    nobs <- nrow(y)
    
    if (nobs > threshold){  
      r <- c(r, cor(y$sulfate, y$nitrate))
    }  
    
  }
  
  
  if(is.null(r)){
    r <- 0
  }
  
  setwd(p)
  
  r
  
  
}
