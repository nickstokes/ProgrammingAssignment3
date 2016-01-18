best <- function(state, outcome){
        ## Read outcome data
        outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        states <- unique(outcomes$State)
        conditions <- c('heart attack','heart failure', 'pneumonia')
        columns <- c(11, 17, 23)

        if(sum(states == state) != 1){
                stop("invalid state")
        }
        if(sum(conditions == outcome) != 1){
                stop("invalid outcome")
        }
        
        ## store column number of outcome
        selectedCol <- columns[conditions == outcome]
        
        ## subset of required columns and state
        statedata <- outcomes[outcomes$State == state,c(2,7,selectedCol)]
        
        ## coerce as numeric
        statedata[,3] <- as.numeric(statedata[,3])
        statedata <- statedata[!is.na(statedata[,3]),]
        
        ## find best rate
        bestRate <- min(statedata[,3])
        
        ## filter to hospitals names with best rate
        statedata <- statedata[statedata[,3] == bestRate,1]
        
        ## sort and return first result
        sort(statedata)[1]
}