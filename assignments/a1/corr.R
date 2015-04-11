corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of lenght 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    ## Get complete cases
  
    files <- list.files(path=directory)
    data <- data.frame()
 
    complete_cases = complete(directory)
    size = dim(complete_cases)[[1]]
    
    corr_matrix <- vector()
    count <- 0
    
    for(i in 1:size) {
        if(complete_cases[i,"nobs"] > threshold) {
            
            # Get file identifier
            file_name <- paste(directory, "/", files[[i]], sep="")
            data <- read.csv(file_name)[2:3]
            good <- complete.cases(data)
            good_data <- data[good,]
            cor_val <- cor(good_data["sulfate"], good_data["nitrate"])
            count <- count + 1
            corr_matrix[count] <- cor_val
        }
    }

    corr_matrix
    
}