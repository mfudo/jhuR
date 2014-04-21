corr <- function(directory, threshold = 0) {
    files <- list.files(path=directory, pattern="*.csv")
    vals <- numeric()
    for (file in files) {  
        path = paste(directory, file, sep = "/")
        data <- read.csv(path, header = TRUE)
        comp <- complete.cases(data)
        if (sum(comp) > threshold){
            thisCor <- cor(data$sulfate, data$nitrate, use = "pairwise.complete.obs")
            vals <- c(vals, thisCor)
        }
    }
    round(vals, 4)
}
