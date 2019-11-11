rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    valid_states <- unique(data$State)
    if (!is.element(state, valid_states))
        stop("invalid state")
    
    valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!is.element(outcome, valid_outcomes))
        stop("invalid outcome")
    
    
    ## Return hospital name in that state with the given rank 30-day death rate
    ## 30 day mortality - Heart Attack  : 11
    ## 30 day mortality - Heart Failure : 17
    ## 30 day mortality - Pneumonia     : 23
    
    ## Way to map the string argument 'outcome' to  the column index
    col_indexes <- c(11, 17, 23)
    names(col_indexes) <- valid_outcomes
    
    ## Subsetted dataframe with only the rows in the specified state
    df_state <- data[data$State == state,]
    
    ## Check if num argument is a numeric
    ## If so, check num < # hospitals
    if (class(num) == "numeric") {
        if (num > length(df_state))
            stop(NA)
    }
    
    ## Get the dataframe's column number of the specified outcome
    col_num <- col_indexes[[outcome]]
    
    ## Get the dataframe's column name of the specified outcome
    colname_outcome <- colnames(data)[col_num]
    
    ## Get the column of interest
    col_outcome <- as.numeric(df_state[, col_num])
    # remove <- c("Not Available")
    # col_outcome <- as.numeric(col_outcome[!col_outcome %in% remove])

    
    
    ## Convert the mortatlity rate from heart attack, heart failure, and
    ## pneumonia columns to numeric vectors
    df_state[, col_indexes] <- sapply(df_state[, col_indexes], as.numeric)
    
    ## Order the subsetted dataframe based on the argument passed
    ## (break ties with lexicographical ordering of hospital name)
    df_state <- df_state[order(df_state[[colname_outcome]], df_state$Hospital.Name),]
    
    ## Take out rows that have NA (coerced from "Not Available")
    ## in the column of interest
    df_state <- df_state[!is.na(df_state[[colname_outcome]]),]
    
    
    hospital_name <- character()
    ## Prepare return value
    if (num == "best") {
        hospital_name <- head(df_state, 1)[1,2]
    } else if (num == "worst") {
        hospital_name <- tail(df_state, 1)[1,2]
    } else {
        hospital_name <- head(df_state, num)[num,2]
    }

    hospital_name
    
}