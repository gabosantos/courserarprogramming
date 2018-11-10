corr <- function(directory, threshold = 0) {
     # Read all files in a directory
     filenames <- list.files(path = directory)
     
     #Initialize a working data frame
     my_data <- c()
     
     # Fill in the data frame with values from CSV files
     for (filename in filenames) {
          # print(filename)
          specdata <- read.csv(paste0(directory, "/", filename))

          #Get the complete cases in the file
          new_data <- data.frame()
          new_data <- complete.cases(specdata)
          
          #If no. of complete cases meet threshold, calculate correlation for that file and add it to the vector
          if (sum(new_data, na.rm = TRUE) > threshold) {
               my_data <- c(my_data, cor(specdata$sulfate, specdata$nitrate, use="complete.obs"))
          }
     }
     
     my_data
}
