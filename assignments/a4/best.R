best <- function(state, outcome) {
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
    
    outcome_names <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
        "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
        "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
    
    
    valid_measures <- measures_for_a_state[, outcome_names]
    best <- data.frame()
    best[,1] <- measures_for_a_state$Hospital.Name
    
    if(outcome == "heart attack") {
        best$mortality <- measures_for_a_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack        
        
        
    } else if(outcome == "heart failure") {
        best$mortality <- measures_for_a_state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
        
        
    } else {
        best <- measures_for_a_state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
        
    }
    best
    #clean_data <- complete_cases[best]
    
    #max <- max(as.numeric(measures_for_a_state[,1], rm.na=T))
    
    
    #max
    
    ## valid_states <- outcome[state == states]
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
}