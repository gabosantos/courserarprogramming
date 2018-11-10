pollutantmean <- function(directory, pollutant, id = 1:332) {
        # Initialize a data frame
        my_data <- data.frame()
        
        # Fill in the data frame with values from CSV files
        for (i in id) {
                if (i < 10)     i <- paste0("00",i)
                else if (i < 100) i <- paste0("0",i)
                else i
                
                specdata <- read.csv(paste0(directory,"/",i,".csv"))
                my_data <- rbind(my_data,specdata)
                specdata <- data.frame()
        }

        # Return mean of nitrate or sulfate column, ignoring NA
        if(pollutant == "nitrate") 
                mean(my_data$nitrate, na.rm=TRUE)
        else {
                ppm <- my_data$sulfate
                mean(ppm[!is.na(ppm)])
        }
}

