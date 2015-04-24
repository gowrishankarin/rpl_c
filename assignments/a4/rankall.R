rankall <- function(outcome, num = "best") {
    
    library(data.table)
    
    outcome <- tolower(outcome)
    ## Read outcome data
    care_measures <- read.csv("./data/outcome-of-care-measures.csv", na.strings="Not Available")
    
    ## Check that sate and outcome are valid
    ##states <- care_measures$State
    ## Is state is in states
    ##if(sum(as.numeric(care_measures$State[state == care_measures$State])) == 0 )
      ##  stop("invalid state")
    
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
    output <- data.frame(state = state_list)
    rownames(output) <- state_list

    res <- split(best, best$State)
    
    for(i in 1:length(res)) {
        output[res[1], "hospital"] <- rank(res[[i]])
    }
    
    output
    
}

rank <- function(best) {
    result <- best[order(best$mortality),]
    
    if(num == "worst")
        num <- dim(result)[1]
    
    if(num == "best")
        num = 1
    
    output <- NA
    if(num <= dim(result)[[1]])
        output <- as.character(result[[1]][num])
    
    output
}


