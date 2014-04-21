complete <- function(directory, id = 1:332) {
    d <- data.frame("id" = numeric(0), "nobs" = numeric(0))
    for (i in id) {   
        fl <- sprintf("%03d.csv", i)
        path <- paste(directory, fl, sep = "/")
        data <- read.csv(path, header = TRUE)
        comp <- complete.cases(data)
        d <- rbind(d, c("id" = i, "nobs" = sum(comp)))
    }
    colnames(d) <- c("id", "nobs")
    d
}
    
