
rankhospital <- function(state,outcome,num="best"){
  
  state <- as.character(state)
  oc <- as.character(outcome)

  # Read the data
  data <- read.csv("hospital/outcome-of-care-measures.csv",colClasses = "character")
  
  # Check the state and outcome are valid
  
  if(state %in% data$State){
    if(oc %in% c("Heart Attack","Heart Failure","Pneumonia")){
      if(num %in% c("worst","best")||is.numeric(num)){
      
      ## function
      
      colname <- paste0("Hospital 30-Day Death (Mortality) Rates from ",oc)
      colname <- make.names(colname)
      df <- data[which(data$State==state&data[,colname]!="Not Available"),]
      df[,colname] <- as.numeric(df[,colname])
      df <- df[order(df$Hospital.Name),]
      if(num=="worst"){
        df <- df[order(df[,colname],decreasing = T,na.last = T),]
      }
      else{
         df <- df[sort.list(df[,colname],method="radix"),]
      }
      
      rowindex <- ifelse(is.numeric(num),num,1)
      h <- df[rowindex,2]
      print(h)
      
    }
    else{
      print("Invalid num")
    }
    }
    else{
      writeLines("Invalid outcome \nPossible outcomes - Heart Attack,Heart Failure,Pneumonia")
    }
  } 
  else{
    print("invalid state")
  }
}

rankhospital("MD","Heart Failure",5)

