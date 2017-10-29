rm(list=ls())

options(warn=-1) #Turn warning off

#Write a function called best that take two arguments: the 2-character abbreviated name of a state and an
#outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
#with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
#in that state. The hospital name is the name provided in the Hospital.Name variable. The outcomes can
#be one of “heart attack”, “heart failure”, or “pneumonia”. Hospitals that do not have data on a particular
#outcome should be excluded from the set of hospitals when deciding the rankings.


best <- function(state, outcome){
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
     return(as.character(list[which.min(list[,3]),][1,1]))
}

##ANSWER CHECK
best("TX", "heart attack") #Result: [1] "CYPRESS FAIRBANKS MEDICAL CENTER", CORRECT
best("TX", "heart failure") #Result: [1] "FORT DUNCAN MEDICAL CENTER", CORRECT
best("MD", "heart attack") #Result: [1] "JOHNS HOPKINS HOSPITAL, THE", CORRECT
best("MD", "pneumonia") #Result: [1] "GREATER BALTIMORE MEDICAL CENTER", CORRECT
