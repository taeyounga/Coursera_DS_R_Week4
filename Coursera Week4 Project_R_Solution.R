rm(list=ls())

install.packages("data.table")
library(data.table)
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



rankall <- function(outcome, num = "best") {
     df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     df_extract <- as.data.frame(cbind("hospital" = df[,2],"State"= df[,7],"heart attack"=as.numeric(df[,11]),"heart failure"=as.numeric(df[,17]), "pneumonia"=as.numeric(df[,23])))
     df_extract[,3] <- as.numeric(as.character(df_extract[,3]))
     df_extract[,4] <- as.numeric(as.character(df_extract[,4]))
     df_extract[,5] <- as.numeric(as.character(df_extract[,5]))
     
     ## Check that outcome is valid
     if(!is.element(outcome, names(df_extract))){
          return(paste("Input outcome",outcome,"is invalid.",sep=" "))
     }else{
          df <- df_extract[,c("hospital","State",outcome)]
          df <- df[order(df[,2], df[,3], df[,1]),]
          state_list <- sort(unique(df[,2]))
     }
     
     
     k <- 0
     for(i in 1:length(state_list)){
          for(j in 1:nrow(df[df$State==state_list[i],])){
               df$Rank[j+k] <- j
          }
          k <- k + nrow(df[df$State==state_list[i],])
     }
     
     State <- state_list
     State <- as.data.frame(State)
     
     if(num=="best"){
          num <- 1
          output <- merge(x = df[df$Rank==num,], y = State, by = "State", all = TRUE)
          return(output[,1:2])
     }
     else if(num=="worst"){
          df <- df[complete.cases(df),]
          output <- setDT(df)[, .SD[which.max(eval(parse(text = outcome )))], by=State]
          output <- as.data.frame(output)
          return(output[,1:2])
     }
     
     else{
          output <- merge(x = df[df$Rank==num,], y = State, by = "State", all = TRUE)
          return(output[,1:2])
     }
     
     
}

##ANSWER CHECK
head(rankall("heart attack", 20),10) #CORRECT
tail(rankall("pneumonia", "worst"), 3) #CORRECT
tail(rankall("heart failure"), 10) #CORRECT

##COURSERA WEEK4 QUIZ:
best("SC", "heart attack")
best("NY", "pneumonia")
best("AK", "pneumonia")
rankhospital("NC", "heart attack", "worst")
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
r <- rankall("heart attack", 4)
r
as.character(subset(r, State == "HI")$hospital)
r <- rankall("pneumonia", "worst")
as.character(subset(r, State == "NJ")$hospital)
r <- rankall("heart failure", 10)
as.character(subset(r, State == "NV")$hospital)

##RESULT: 100%
