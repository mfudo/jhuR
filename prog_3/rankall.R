# Programming assignment Part 3
# Find the hospital for each state according to user specified
# rank and outcome.

setwd("~/jhuR/prog_3/")

library(Hmisc)

# Find the best hospital by state and health condition
# using the 30 day mortality
rankall <- function(outcome, num="best") { 
    # List of the health conditions that are allowed
    outcomes <- c("heart attack", "heart failure", "pneumonia") 
    # List of states
    states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT",
                "DE", "FL", "GA", "HI", "ID", "IL", "IN",
                "IA", "KS", "KY", "LA", "ME", "MD", "MA",
                "MI", "MN", "MS", "MO", "MT", "NE", "NV",
                "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
                "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                "WY", "VI", "DC", "GU", "MP", "PR")
    
    if (!(outcome %in% outcomes)){ # Allowed condition?
        stop("invalid outcome")
    }
    else {
        # Process to match the user input to the column names
        outcome <- strsplit(outcome, " ")[[1]]
        outcome <- paste(capitalize(outcome), collapse=".")
        # Read in the data
        care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        # Create an empty data frame to hold the results from each state
        results <- data.frame("hospital"=character(), "state"=character(), stringsAsFactors=F)
        # Get the column index for the user-specified condition
        searchExp <- paste("^Hospital.+30.+Day.+Death.+Mortality.+Rates.+", outcome, sep="")
        colNum<- grep(searchExp, colnames(filtered))
        
        for (st in states){
            filtered <- care[care$State == st,]
            # Coerce the 30 day mortality column to numeric
            filtered[,colNum] <- as.numeric(filtered[,colNum], warn=-1)
            filtered[,2] <- as.character(filtered[,2])
            # Sort by the outcome ascending and return the name of 
            # the hospital
            sorted <- filtered[order(filtered[,colNum], filtered[,2]), c(colNum, 2)]
            cleaned <- na.omit(sorted)
            
            if (num == "best"){
                ind = 1
            }
            else if (num == "worst"){
                ind = nrow(cleaned)
            }
            else {
                ind = num
            }
            results[nrow(results)+1,] <- c(cleaned[ind,2], as.character(st))
        }
        names(results) <- c('hospital', 'state')
        results[order(results$state),] 
    }
}

