corr <- function(directory, threshold = 0) {
  x <- dir("./specdata")
  i <- 0
  k <- 0
  corrl <- c()
  for (id in x) {
    i <- i + 1
    data <- read.csv(paste0("./specdata/", x[i]))
    nobs <- nrow(subset(data, !is.na(data$sulfate) & !is.na(data$nitrate)))
    if (nobs > threshold) {
      k <- k + 1
      corrl[k] <- cor(data$sulfate, data$nitrate, use = "na.or.complete")
    }
  }
  corrl
}