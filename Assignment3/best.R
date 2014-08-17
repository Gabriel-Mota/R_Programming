best <- function(state, outcome){
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if(outcome == "heart attack"){
    aux <- subset(data, select = c(2, 7, 11))
  }
  else{
    if(outcome == "heart failure"){
      aux <- subset(data, select = c(2, 7, 17))
    }
    else{
      if(outcome == "pneumonia"){
        aux <- subset(data, select = c(2, 7, 23))
      }
      else{
        stop("invalid outcome")
      }
    }
  }
  
  if(!(state %in% aux[,2]))
    stop("invalid state")
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  t <- subset(aux, State == state, select = c(1,2,3))
  t[,3] <- as.numeric(t[,3])
  t<-t[!is.na(t[,3]),]
  min <- min(t[,3])
  t[t[,3] == min, 1]
  
}

