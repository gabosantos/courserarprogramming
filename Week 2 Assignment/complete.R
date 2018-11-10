complete <- function (directory, id = 1:332) {
        complete_data <- data.frame()
        
        # Fill in the data frame with values from CSV files
        for (i in id) {
                if (i < 10)     ii <- paste0("00",i)
                else if (i < 100) ii <- paste0("0",i)
                else ii <- i
                
                my_data <- data.frame()
                
                specdata <- read.csv(paste0(directory,"/",ii,".csv"))
                my_data <- rbind(my_data,specdata)
                specdata <- data.frame()
                
                # Fill in new_data data frame with complete cases only
                new_data <- data.frame()
                new_data <- complete.cases(my_data) 
                
                
                # Getting the sum of TRUEs or 1s in new_data
                complete_data <- rbind(complete_data, c(i,sum(new_data, na.rm=TRUE)))
        }
        
        colnames(complete_data) <- c("id","nobs")
        complete_data

}
