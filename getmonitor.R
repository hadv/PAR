getmonitor <- function(id, directory, summarize = FALSE) {
  idx <- as.integer(id)
  x <- dir("./specdata")
  data <- read.csv(paste0("./specdata/", x[idx]))
  if (summarize == TRUE) {
    print(summary(data))
  }
  data
}