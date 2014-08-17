
best <- function(state, outcome) {
    ## Read outcome data
    outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    # Column key for outcome.measures date frame
    #  2 "hospital"
    #  7 "state"
    # 11 "heart attack"
    # 17 "heart failure"
    # 23 "pneumonia"
    cols <- c("hospital" = 2,
             "state" = 7,
             "heart attack" = 11,
             "heart failure" = 17,
             "pneumonia" = 23
             )

    ## Check that state and outcome are valid
    valid.states <- levels(factor(outcomes$State))
    valid.outcomes <- names(cols[3:length(cols)])

    if ( !(state %in% valid.states) ) {
        #print("Please enter one of the following states used in the dataset.")
        #print(valid.states)
        stop("invalid state")
    } else if ( !(outcome %in% valid.outcomes) ) {
        #print("Please enter one of the following outcomes available in the dataset.")
        #print(valid.outcomes)
        stop("invalid outcome")
    }

    ## Return hospital name in that state with lowest 30-day death rate

    ## Build a logical vector of valid rows
    #rows <- outcomes[cols["state"]] == state & !is.na(as.numeric(outcomes[,cols[outcome]]))
    rows <- outcomes[cols["state"]] == state & outcomes[,cols[outcome]] != "Not Available"
    ## Build a simplified data frame with the specified criteria and valid values
    results <- outcomes[ rows, c(cols["hospital"], cols[outcome]) ]

    ## Rename column names to something short and easy
    colnames(results) <- c("hospital", outcome)
    ## Convert outcome column to numeric
    results[ ,outcome] <- as.numeric(results[ , outcome])

    ## Build a character vector of the name(s) of the hospitals with the lowest outcomes
    best <- results$hospital[ results[ , outcome] == min(results[ , outcome]) ]

    ## Return the first sorted result
    sort(best)[1]
}
