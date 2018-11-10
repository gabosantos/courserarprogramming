rankhospital <- function(state, outcome, num = "best") {
     options(warn = -1)
     
     ## Read outcome data
     data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
     
     ## Check that state and outcome are valid
     validoutcomes <- c("HEART ATTACK", "HEART FAILURE", "PNEUMONIA")
     validstates <- data[,7]
     
     if (is.element(state, validstates) == FALSE)
          stop("invalid state")
     
     if (!is.element(outcome, validoutcomes) == FALSE)
          stop("invalid outcome")
     
     ## Return hospital name in that state with the given rank 30-day death rate
     workingdf <- NULL
     
     if (toupper(outcome) == "HEART ATTACK")
          workingdf <- subset(data, State == state, select = c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
     else if (toupper(outcome) == "HEART FAILURE")
          workingdf <- subset(data, State == state, select = c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
     else if (toupper(outcome) == "PNEUMONIA")
          workingdf <- subset(data, State == state, select = c("State","Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
     
     colnames(workingdf) <- c("State", "Hospital", "Rate")
     workingdf$Rate <- as.numeric(workingdf$Rate)
     workingdf <- workingdf[complete.cases(workingdf),]
     besthospital <- workingdf[order(workingdf$Rate, workingdf$Hospital),2]
     
     if(is.numeric(num) == FALSE)
          if(toupper(num) == "BEST")
               num = 1
          else if(toupper(num) == "WORST")
               num = NROW(besthospital)
     
     besthospital[num]
}