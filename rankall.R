rankall <- function(outcome,num="best"){
  
  oc <- as.character(outcome)
  
  # Read the data
  data <- read.csv("hospital/outcome-of-care-measures.csv",colClasses = "character")
  
  # Check the state and outcome are valid
  
    if(oc %in% c("Heart Attack","Heart Failure","Pneumonia")){
      if(num %in% c("worst","best")||is.numeric(num)){
        
        ## function
        
        l <- as.list(unique(data$State))
        mt <- matrix(nrow=54,ncol=2)
        
        for(i in 1:length(l)){
          
          colname <- paste0("Hospital 30-Day Death (Mortality) Rates from ",oc)
          colname <- make.names(colname)
          
          df <- data[which(data$State==l[[i]]&data[,colname]!="Not Available"),]
          df[,colname] <- as.numeric(df[,colname])
          
          df <- df[order(df$Hospital.Name),]
          
          if(num=="worst"){
            df<- df[order(df[,colname],decreasing = T),]
          }
          else{
            df<- df[order(df[,colname]),]
          }
          rowindex <- ifelse(is.numeric(num),num,1)
          h <- df[rowindex,2]
          mt[i,2]=l[[i]]
          mt[i,1]=h
          
        }
        colnames(mt)<-c("Hospitals","State")
        return(mt)
        
      }
      else{
        print("Invalid num")
      }
    }
    else{
      writeLines("Invalid outcome \nPossible outcomes - Heart Attack,Heart Failure,Pneumonia")
    }
  
}

head(rankall("Heart Attack",20),10)
tail(rankall("Pneumonia","worst"),3)

