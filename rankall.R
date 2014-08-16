rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    source('C:/Users/dba/repos/ProgrammingAssignment3/rankhospital.R')

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
    valid.outcomes <- names(cols[3:length(cols)])

    if ( !(outcome %in% valid.outcomes) ) {
        #print("Please enter one of the following outcomes available in the dataset.")
        #print(valid.outcomes)
        stop("invalid outcome")
    } else if ( !(num %in% c("best", "worst") || (num %% 1 == 0 & num > 0)) ) {
        #print("Please enter one of the following ranks.")
        #cat("\"best\", \"worst\", or the desired numeric rank that is a natural number.")
        stop("invalid rank")
    }

    getRow <- function(state) {
        ## Return hospital name in that state with the given rank 30-day death rate
        ## Build a logical vector of valid rows
        rows <- outcomes[cols["state"]] == state & outcomes[,cols[outcome]] != "Not Available"
        ## Build a simplified data frame with the specified criteria and valid values
        results <- outcomes[ rows, c(cols["hospital"], cols["state"], cols[outcome]) ]

        ## Rename column names to something short and easy
        colnames(results) <- c("hospital", "state", outcome)
        ## Convert outcome column to numeric
        results[ ,outcome] <- as.numeric(results[ , outcome])

        ## Reorder by outcome (then hospital name) and replace dataset
        results <- data.frame( results[order(results[ ,outcome], results$hospital), ] , row.names = 1:nrow(results) )

        ## Let's name the best and worst numerics in our vector instead of using
        ## a conditional testing for the character strings! This way we can just
        ## specify the row we want using the ranks vector. e.g. ranks[num]
        ranks <- c( best = as.numeric(rownames(results[1, ])),
                    seq(from = as.numeric(rownames(results[2, ])), to = nrow(results) - 1),
                    worst = as.numeric(rownames(results[nrow(results), ])))

        ## Finally return the results!
        results[ranks[num], c("hospital", "state")]
    }

    states <- levels(factor(outcomes$State))

    ranked <- data.frame()

    for (s in states) {
        rbind( ranked, getRow(s) )
    }

    rownames(ranked) <- states

    ranked
}
