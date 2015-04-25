rankall <- function(outcome, num = "best") {
    
    library(data.table)
    
    outcome <- tolower(outcome)
    ## Read outcome data
    care_measures <- read.csv("./data/outcome-of-care-measures.csv", na.strings="Not Available")
    
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if(sum(as.numeric(outcome == outcomes)) == 0 )
        stop("invalid outcome")
    
    measures_for_a_state <- care_measures
    
    best <- data.table(hospital = lapply(measures_for_a_state$Hospital.Name, as.character, stringsAsFactor=FALSE))
    best$State <- care_measures$State
    
    if(outcome == "heart attack") {
        best$mortality <- measures_for_a_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack        
        
        
    } else if(outcome == "heart failure") {
        best$mortality <- measures_for_a_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        
        
    } else {
        best$mortality <- measures_for_a_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        
    }
    
    best <- best[complete.cases(best$mortality), ]
    state_list <- sort(unique(best$State))
    
    res <- split(best, best$State)
    
    xx <- matrix(nrow = length(state_list), ncol = 2)
    
    check_dim <- dim(data.table())
    
    for(i in 1:length(res)) {
        
        hospital_name <- rank(res[[i]], num)
        if(!identical(dim(hospital_name), check_dim)) {
            hospital <- as.data.table(hospital_name)
            xx[i, 1] <- as.character(hospital[1, State])
            xx[i, 2] <- as.character(hospital[1, hospital])
        } else {
            xx[i, 1] <- state_list[[i]]
            xx[i, 2] <- NA
        }
    }
    
    yy <- data.frame(hospital = xx[,2], state = state_list)
    rownames(yy) <- state_list
    yy
}

rank <- function(best, num) {
     result <- best[order(best$mortality, best[,1]),]
    
    if(num == "worst")
        num <- dim(result)[1]
    
    if(num == "best")
        num = 1
    
    output <- data.table()
    if(num <= dim(result)[[1]])
        output <- result[num]
    output
    
    
}


