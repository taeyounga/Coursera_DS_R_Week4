rm(list=ls())

options(warn=-1) #Turn warning off

rankhospital <- function(state, outcome, num="best"){
     ## Read outcome data
     df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     df_extract <- as.data.frame(cbind("Hospital name" = df[,2],"State"= df[,7],"heart attack"=as.numeric(df[,11]),"heart failure"=as.numeric(df[,17]), "pneumonia"=as.numeric(df[,23])))
     ## Check that state and outcome are valid
     if(!is.element(state, df_extract[,2])){
          return(paste("Input state",state,"is invalid.",sep=" "))
     }
     else if(!is.element(outcome, names(df_extract))){
          return(paste("Input outcome",outcome,"is invalid.",sep=" "))
     }else{
          df <- df_extract[,c("Hospital name","State",outcome)]
          df <- df[complete.cases(df),]
     }
     ## Return hospital name in that state with lowest 30-day death rate
     list <- subset(df,State == state)
     list[,3] <- as.numeric(as.character(list[,3]))
     list <- list[order(list[,3], list[,1]),]
     list$Rank <- seq.int(nrow(list))
     if(num=="best"){
          return(as.character(list[list$Rank==1,][1,1]))
     }
     else if(num=="worst"){
          return(as.character(list[list$Rank==nrow(list),][1,1]))
     }
     else if(num>nrow(list)){
          return(NA)
     }
     else{
          return(as.character(list[list$Rank==num,][1,1]))
     }
}

##ANSWER CHECK
rankhospital("TX","heart failure", 4) #RESULT: [1] "DETAR HOSPITAL NAVARRO", CORRECT
rankhospital("MD","heart attack", "worst") #RESULT: [1] "HARFORD MEMORIAL HOSPITAL", CORRECT
rankhospital("MN","heart attack", 5000) #RESULT: [1] NA, CORRECT

