rankall <- function(outcome, num = "best") {
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
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  state <- unique(aux[,2])
  state <- state[order(states)]
  len <- length(state)
  i<-1
  hospital<-NULL
  for(i in i:len){
    t <- subset(aux, State == state[i], select = c(1,2,3))
    t[,3] <- as.numeric(t[,3])
    t<-t[!is.na(t[,3]),]
    t<-t[order(t[,3], t[,1]),]
    
    y<-dim(t)
    
    if(num == "worst"){
      num2 <- y[1]
      hospital<-c(hospital, t$Hospital.Name[num2])
    }
    else{
      if(num == "best"){
        num2 <- 1
        hospital<-c(hospital, t$Hospital.Name[num2])
      }
      else{
        hospital<-c(hospital, t$Hospital.Name[num])
      }
    }
  }
  df <- data.frame(hospital, state)
  
  
}
