pollutantmean <- function(directory, pollutant, id=1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
  
	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used

	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
  
  # Get file names
  files <- list.files(path=directory)
  data <- list()
  
  sum <- 0
  count <- 0
  
  # Read required files
  for(i in id) {
    aFile <- paste(directory, "/", files[[i]], sep="")
    data <- read.csv(aFile)[pollutant]
    invalids <- is.na(data)
    valids <- data[!invalids]
    sum <- sum + sum(valids)
    count <- count + length(valids)
  }
  round(sum/count, digits=3)
}