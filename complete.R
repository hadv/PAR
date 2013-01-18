complete <- function(directory, id = 1:332) {
  x <- dir("./specdata")
  nobs <- id
  idx <- 1
  for (i in id) {
    data <- read.csv(paste0("./specdata/", x[i]))
    nobs[idx] = nrow(subset(data, !is.na(data$sulfate) & !is.na(data$nitrate)))
    idx <- idx + 1
  }
  data.frame(id, nobs)
}