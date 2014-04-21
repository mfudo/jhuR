pollutantmean <- function(directory, pollutant, id = 1:332) {
    rs <- numeric(1)
    for (i in id) {   
        fl <- sprintf("%03d.csv", i)
        path <- paste(directory, fl, sep = "/")
        data <- read.csv(path, header = TRUE)
        sel <- data[pollutant]
        rs <- rbind(rs, sel)
    }
    round(mean(rs[,1], na.rm = TRUE), 3)
}
