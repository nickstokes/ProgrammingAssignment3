rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        ## Check that state and outcome are valid
        states <- unique(outcomes$State)
        states <- sort(states)
        conditions <- c('heart attack','heart failure', 'pneumonia')
        columns <- c(11, 17, 23)
        
        ## store column number of outcome
        selectedCol <- columns[conditions == outcome]
        
        ## setup result frame
        result <- data.frame(hospital = character(), state = character(), stringsAsFactors=FALSE)
        
        if(sum(conditions == outcome) != 1){
                stop("invalid outcome")
        }
        
        
        getrank <- function(state, result){
                ## subset of required columns and state
                statedata <- outcomes[outcomes$State == state,c(2,7,selectedCol)]
                
                ## coerce as numeric
                statedata[,3] <- as.numeric(statedata[,3])
                statedata <- statedata[!is.na(statedata[,3]),]
                
                ## order the data
                statedata <- statedata[order(statedata[,3],statedata[,1]),]
                
                if(num == "best"){
                        num <- 1
                }
                if(num == "worst"){
                        num <- nrow(statedata)
                }
                
                ## sort and return first result
                
                c( statedata[num,1], state)
        }
        
        for(currentstate in states){
                currentresult <- getrank(currentstate, result)
                newrow <- nrow(result)+1
                result[newrow,] <- currentresult
        }
        result
        
}