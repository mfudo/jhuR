# Programming assignment 3
setwd("~/jhuR/prog_3/")

library(Hmisc)

best <- function(state, outcome) { 
        outcomes <- c("heart attack", "heart failure", "pneumonia") 
        states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT",
                    "DE", "FL", "GA", "HI", "ID", "IL", "IN",
                    "IA", "KS", "KY", "LA", "ME", "MD", "MA",
                    "MI", "MN", "MS", "MO", "MT", "NE", "NV",
                    "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
                    "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                    "TX", "UT", "VT", "VA", "WA", "WV", "WI",
                    "WY")
    
        if (!(state %in% states)){
            stop("invalid state")
        }
        else if (!(outcome %in% outcomes)){
            stop("invalid outcome")
        }
        else {
            outcome <- strsplit(outcome, " ")[[1]]
            outcome <- paste(capitalize(outcome), collapse=".")
            care <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
            filtered <- care[care$State == state,]
            searchExp <- paste("Lower.+Mortality.+Estimate.+", outcome, sep="")
            colNum<- grep(searchExp, colnames(filtered))
            
            filtered[,colNum] <- as.numeric(filtered[,colNum], warn=-1)
            sorted <- filtered[order(filtered[,colNum]),]
            as.character(sorted[1,2])
            }
        }
