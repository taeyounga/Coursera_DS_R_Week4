rm(list=ls())

install.packages("data.table")
library(data.table)
options(warn=-1) #Turn warning off

rankall <- function(outcome, num = "best") {
     df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
     
     df_extract <- as.data.frame(cbind("Hospital name" = df[,2],"State"= df[,7],"heart attack"=as.numeric(df[,11]),"heart failure"=as.numeric(df[,17]), "pneumonia"=as.numeric(df[,23])))
     df_extract[,3] <- as.numeric(as.character(df_extract[,3]))
     df_extract[,4] <- as.numeric(as.character(df_extract[,4]))
     df_extract[,5] <- as.numeric(as.character(df_extract[,5]))
     
     ## Check that outcome is valid
     if(!is.element(outcome, names(df_extract))){
          return(paste("Input outcome",outcome,"is invalid.",sep=" "))
     }else{
          df <- df_extract[,c("Hospital name","State",outcome)]
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