rankall <- function(outcome, num = "best") {
     options(warn = -1)
     
     ## Read outcome data
     data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
     
     ## Check that outcome is valid
     validoutcomes <- c("HEART ATTACK", "HEART FAILURE", "PNEUMONIA")
     
     if (!is.element(outcome, validoutcomes) == FALSE)
          stop("invalid outcome")
     
     ## For each state, find the hospital of the given rank
     workingdf <- NULL
     
     if (toupper(outcome) == "HEART ATTACK")
          workingdf <- subset(data, select = c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
     else if (toupper(outcome) == "HEART FAILURE")
          workingdf <- subset(data, select = c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
     else if (toupper(outcome) == "PNEUMONIA")
          workingdf <- subset(data, select = c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
     

     colnames(workingdf) <- c("State", "Hospital", "Rate")
     workingdf$Rate <- as.numeric(workingdf$Rate)
     workingdf <- workingdf[complete.cases(workingdf),]
     besthospital <- workingdf[order(workingdf$State, workingdf$Rate, workingdf$Hospital),]
     
     rankeddf <- NULL
     uniquestates <- unique(data[,7])
     
     for (i in 1:length(uniquestates)) {
          tempdf <- subset(besthospital, State == uniquestates[i], select = c("Hospital", "State", "Rate"))
          tempdf <- as.data.frame(tempdf)
          
          tempdf$Rank <- NA
          tempdf$Rank[order(tempdf$Rate)] <- 1:nrow(tempdf)
          
          rankeddf <- rbind(rankeddf, tempdf)
     }
     colnames(rankeddf) <- c("Hospital", "State", "Rate", "Rank")
     
     finaldf <- NULL
     if(is.numeric(num) == FALSE) {
          if(toupper(num) == "BEST") {
               num = 1
               finaldf <- subset(rankeddf, Rank == num, select = c("Hospital", "State"))
          }
          else if(toupper(num) == "WORST") {
               for (i in 1:length(uniquestates)) {
                    tempdf2 <- subset(rankeddf, State == uniquestates[i], select = c("Hospital", "State"))
                    tempdf2 <- as.data.frame(tempdf2)
                    
                    num = NROW(tempdf2)
                    finaldf <- rbind(finaldf, tempdf2[num,])
               }
          }
     }
     else if(is.numeric(num) == TRUE){
          for (i in 1:length(uniquestates)) {
               tempdf2 <- subset(rankeddf, State == uniquestates[i], select = c("Hospital", "State"))
               tempdf2 <- as.data.frame(tempdf2)
               
               #num = NROW(tempdf2)
               finaldf <- rbind(finaldf, c(tempdf2[num,1], uniquestates[i]))
          }
     }
     
     ## Return a data frame with the hospital names and the abbreviated state name
     
     finaldf <- as.data.frame(finaldf)
     colnames(finaldf) <- c("Hospital", "State")
     rownames(finaldf) <- finaldf$State
     finaldf <- finaldf[order(finaldf$State),]
     finaldf
     
     
     
}