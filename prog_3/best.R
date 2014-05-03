# Programming assignment 3
setwd("~/jhuR/prog_3/")

library(Hmisc)

# Find the best hospital by state and health condition
# using the 30 day mortality
best <- function(state, outcome) { 
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
                    "WY")
    
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
            searchExp <- paste("Lower.+Mortality.+Estimate.+", outcome, sep="")
            colNum<- grep(searchExp, colnames(filtered))
            # Coerce the 30 day morbidity column to numeric
            filtered[,colNum] <- as.numeric(filtered[,colNum], warn=-1)
            # Sort by the outcome ascending and return the name of 
            # the hospital
            sorted <- filtered[order(filtered[,colNum]),]
            as.character(sorted[1,2])
            }
        }
