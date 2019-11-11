rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, valid_outcomes))
        stop("invalid outcome")
    
    ## Split data into groups by state
    df_state_split <- split(data, data$State)
    
    ## Way to map the string argument 'outcome' to  the column index
    col_indexes <- c(11, 17, 23)
    names(col_indexes) <- valid_outcomes
    
    ## Get the dataframe's column number of the specified outcome
    col_num <- col_indexes[[outcome]]
    
    ## Get the dataframe's column name of the specified outcome
    colname_outcome <- colnames(data)[col_num]
    
    ## Get the column of interest
    col_outcome <- as.numeric(data[, col_num])
    col_outcome <- col_outcome[!is.na(col_outcome)]
    
    ## Order all the states to bind with outcome data
    states <- unique(data$State)
    state_ordering <- order(states)
    states <- states[state_ordering]
    
    outcome_data <- numeric()
    ## For each state, find the hospital of the given rank
    if (num == "best")
        outcome_data <- lapply(df_state_split, function(x) {
            
            # Find the lowest number of the outcome of interest
            min_num <- min(as.numeric(x[[colname_outcome]]), na.rm = TRUE)
            
            ## Match the lowest number with the hospital name
            min_hospital <- x[as.numeric(x[[colname_outcome]]) == min_num, 2]
            
            ## min_hospital is a character vector with NA values, so we
            ## extract the hospital name
            ## It is possible that the whole vector is NA's, in the event
            ## of a state not having data for that state+outcome
            if (all(is.na(min_hospital))) {
                min_hospital <- NA
            } else {
                min_hospital <- min_hospital[!is.na(min_hospital)]
            }
            
            min_hospital
            
            })
    else if (num == "worst")
        outcome_data <- lapply(df_state_split, function(x) {
            ## Refer to if clause above
            max_num <- max(as.numeric(x[[colname_outcome]]), na.rm = TRUE)

            max_hospital <- x[as.numeric(x[[colname_outcome]]) == max_num, 2]
            
            if (all(is.na(max_hospital))) {
                max_hospital <- NA
            } else {
                max_hospital <- max_hospital[!is.na(max_hospital)]
            }
            
            max_hospital
            
            })
    else {
        ## Order the data (decreasing = FALSE) by the outcome value, and use
        ## the hospital name to break ties
        ## Index to get the num'th value and the 2nd column (hospital.name)
        outcome_data <- lapply(df_state_split, function(x) x[order(x[[colname_outcome]], x$Hospital.Name),][num,2] )
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    cbind(hospital=outcome_data, state=states)
}
