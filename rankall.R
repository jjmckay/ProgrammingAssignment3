rankall2 <- function(outcome, num = "best") {
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

    ## Check that outcome and num are valid
    validOutcomes <- names(cols[3:length(cols)])

    if ( !(outcome %in% validOutcomes) ) {
        #print("Please enter one of the following outcomes available in the dataset.")
        #print(valid.outcomes)
        stop("invalid outcome")
    } else if ( !(num %in% c("best", "worst") || (num %% 1 == 0 & num > 0)) ) {
        #print("Please enter one of the following ranks.")
        #cat("\"best\", \"worst\", or the desired numeric rank that is a natural number.")
        stop("invalid rank")
    }

    ## Get the list of states present in the dataset in order
    states <- levels(factor(outcomes[ , cols["state"]]))

    ## Build a logical vector that drops any outcome listed as "Not Available"
    rowsPopulated <- outcomes[ ,cols[outcome]] != "Not Available"

    ## Pare down the dataset to just the columns we need: hospital, state, outcome
    outcomesPared <- data.frame(outcomes[rowsPopulated ,c(cols["hospital"],
                                                          cols["state"],
                                                          cols[outcome])])
    ## Rename columns to simpler names for easy referral
    colnames(outcomesPared) <- c("hospital", "state", outcome)

    ## Convert state column to a type of factor
    outcomesPared$state <- factor(outcomesPared$state)

    outcomesPared[ , outcome] <- as.numeric(outcomesPared[ , outcome])

    ## Reorder the rows first by state then by outcome then by hospital name
    outcomesPared <- outcomesPared[order(outcomesPared[ , "state"],
                                         outcomesPared[ , outcome],
                                         outcomesPared[ , "hospital"]), ]

    ## Reindex the rows in sequential order
    rownames(outcomesPared) <- 1:nrow(outcomesPared)

    ## Adds a column of state ranks for hospitals
    outcomesPared$state.rank <- sequence(tabulate(outcomesPared$state))

    #### A function to retrieve and return the rank num specifies of each state
    getRankedRow <- function(stateFrame, num = num) {
        state <- as.character(stateFrame[stateFrame$state.rank == 1, "state"])

        if (num == "best") {
            returnRow <- stateFrame[ stateFrame$state.rank == 1, ]
        } else if (num == "worst") {
            returnRow <- stateFrame[ stateFrame$state.rank == nrow(stateFrame), ]
        } else if (1 <= num && num < nrow(stateFrame)) {
            returnRow <- stateFrame[ stateFrame$state.rank == num, ]
        } else {
            returnRow <- data.frame(rbind(c(hospital = "<NA>", state = state)))
        }

        rownames(returnRow) <- state
        returnRow <- returnRow[ , c("hospital", "state")]

        returnRow
    }

    outcomesList <- split(outcomesPared, outcomesPared$state)

    results <- lapply(outcomesList, getRankedRow, num = num)
    results <-  do.call("rbind", results)

    results
}
