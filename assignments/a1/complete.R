complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    # Get file names
    files <- list.files(path=directory)
    data <- list()
    
    sum <- 0
    count <- 0
    
    complete_cases <- data.frame()
    
    # Read required files
    for(i in id) {
        aFile <- paste(directory, "/", files[[i]], sep="")
        data <- read.csv(aFile)
        
        good <- complete.cases(data)
        valids <- data[good,]
        
        size <- dim(valids)[[1]]
        
        complete_cases[i,1] = i
        complete_cases[i,2] = size
        
    }
    good <- complete.cases(complete_cases)
    valids <- complete_cases[good,]
    colnames(valids) <- c("id", "nobs")
    
    output = valids
    if(id[[1]] > id[[2]]) {
      output = valids[order(-valids[1]),]
    } else {
      output = valids[order(valids[1]),]
    }
    
    
    rownames(output)<- c(1:dim(output)[[1]])
    output
}