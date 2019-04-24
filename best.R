getwd()
outcome <- read.csv("hospital/outcome-of-care-measures.csv",colClasses = "character")
head(outcome)

####
outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

####

best <- function(state,outcome){
  
  state <- as.character(state)
  oc <- as.character(outcome)

  # Read the data
  outcome <- read.csv("hospital/outcome-of-care-measures.csv",colClasses = "character")
  
  # Check the state and outcome are valid
  
  if(state %in% outcome$State){
    if(oc %in% c("Heart Attack","Heart Failure","Pneumonia")){
      
      ## function
      
      colname <- paste0("Hospital 30-Day Death (Mortality) Rates from ",oc)
      colname <- make.names(colname)
      df <- outcome[which(outcome$State==state),]
      df <- df[order(df$Hospital.Name),]
      rowindex <- suppressWarnings(which.min(df[,colname]))
      h <- df[rowindex,2]
      print(h)
      
    }
    
    else{
      writeLines("Invalid outcome \nPossible outcomes - Heart Attack,Heart Failure,Pneumonia")
    }
    
  } 
  
  else{
    print("invalid state")
  }
}


best("BB","Heart Attack")


