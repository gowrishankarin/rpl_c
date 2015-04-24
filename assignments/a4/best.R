best <- function(state, outcome) {
    library(data.table)
    
    outcome <- tolower(outcome)
    ## Read outcome data
    care_measures <- read.csv("./data/outcome-of-care-measures.csv", na.strings="Not Available")
    
    ## Check that sate and outcome are valid
    states <- care_measures$State
    ## Is state is in states
    if(sum(as.numeric(care_measures$State[state == care_measures$State])) == 0 )
        stop("invalid state")
    
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if(sum(as.numeric(outcome == outcomes)) == 0 )
        stop("invalid outcome")
    
    measures_for_a_state <- care_measures[which(care_measures$State == state),]
    
    best <- data.table(lapply(measures_for_a_state$Hospital.Name, as.character, stringsAsFactor=FALSE))
    
    if(outcome == "heart attack") {
        best$mortality <- measures_for_a_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack        
        
        
    } else if(outcome == "heart failure") {
        best$mortality <- measures_for_a_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        
        
    } else {
        best$mortality <- measures_for_a_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        
    }
    result <- best[which.min(best$mortality)]
    as.character(result[,V1])
    
    
}