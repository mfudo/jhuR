# Programming assignment Part 3
# Find the hospital according to user specified
# rank, outcome, and state.

setwd("~/jhuR/prog_3/")

library(Hmisc)

# Find the best hospital by state and health condition
# using the 30 day mortality
rankhospital <- function(state, outcome, num="best") { 
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
    
    if (!(state %in% states)){ # Is it a real state
        stop("invalid state")
    }
    else if (!(outcome %in% outcomes)){ # Allowed condition?
        stop("invalid outcome")
    }
    else {
        # Process to match the user input to the column names
        outcome <- strsplit(outcome, " ")[[1]]
        outcome <- paste(capitalize(outcome), collapse=".")
        care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        filtered <- care[care$State == state,]
        searchExp <- paste("^Hospital.+30.+Day.+Death.+Mortality.+Rates.+", outcome, sep="")
        colNum<- grep(searchExp, colnames(filtered))
        # Coerce the 30 day mortality column to numeric
        filtered[,colNum] <- as.numeric(filtered[,colNum], warn=-1)
        # Sort by the outcome ascending and return the name of 
        # the hospital
        sorted <- filtered[order(filtered[,colNum], filtered[,2]),]
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
        as.character(cleaned[ind,2])
    }
}
