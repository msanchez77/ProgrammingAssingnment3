best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    valid_states <- unique(data$State)
    if (!is.element(state, valid_states))
        stop("invalid state")
    
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, valid_outcomes))
        stop("invalid outcome")
    
    ## Return hospital name in that state with lowest 30-day death rate
    ## 30 day mortality - Heart Attack  : 11
    ## 30 day mortality - Heart Failure : 17
    ## 30 day mortality - Pneumonia     : 23
    
    ## Way to map the string argument 'outcome' to  the column index
    col_indexes <- c(11, 17, 23)
    names(col_indexes) <- valid_outcomes
    
    ## Subsetted dataframe with only the rows in the specified state
    df_state <- data[data$State == state,]
    
    ## Get the dataframe's column number of the specified outcome
    colnum <- col_indexes[[outcome]]
    
    ## Get the dataframe's column name of the specified outcome
    colname_outcome <- colnames(data)[colnum]

    
    ## Get the column of interest
    col_outcome <- df_state[, colnum]
    remove <- c("Not Available")
    col_outcome <- as.numeric(col_outcome[!col_outcome %in% remove])
    
    ## Numeric of the minimum value of the specified outcome
    # cat("Examining column", colnum, "...\n")
    # cat(col_outcome, "\n", sep = "\t")
    min_hospitals_outcome <- min(col_outcome, na.rm = TRUE)

    
    # cat("Best", colname_outcome, "in", state, ":", min_hospitals_outcome, "\n")
    
    ## Get the row(s) of the hospitals with the minimum value
    min_hospital <- df_state[as.numeric(df_state[[colname_outcome]]) == min_hospitals_outcome, 2]
    
    min_hospital <- min_hospital[!is.na(min_hospital)]
    
    min_hospital
}

# print(best("TX", "heart attack"))
# print(best("TX", "heart failure"))
# print(best("MD", "heart attack"))
# print(best("MD", "pneumonia"))