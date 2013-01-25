rankhospital <- function(state, outcome, num = "best")
{
  ## read outcome data
  outcome_data <- read.csv("./ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  suppressWarnings(outcome_data[, 11] <- as.numeric(outcome_data[, 11]) )
  suppressWarnings(outcome_data[, 17] <- as.numeric(outcome_data[, 17]) )
  suppressWarnings(outcome_data[, 23] <- as.numeric(outcome_data[, 23]) )
  
  ## Check that state and outcome are valid
  
  table_data <- data.frame(State=names(tapply(outcome_data$State, outcome_data$State, length)),
                         Freq=tapply(outcome_data$State, outcome_data$State, length))
  rownames(table_data) <- NULL
  
  input_data <- data.frame(Outcome=c("heart attack", "heart failure","pneumonia"), Col=c(11, 17, 23))
  
  if( nrow(table_data[table_data$State==state,]) == 0 ) stop( "invalid state" )
  if( nrow(input_data[input_data$Outcome==outcome,]) == 0 ) stop( "invalid outcome" )
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  state_data <- outcome_data[outcome_data$State==state, ]
  colNum <- input_data[input_data$Outcome==outcome, 2]
  state_data <- state_data[complete.cases(state_data[, colNum]), ]
  state_data <- state_data[order(state_data[, colNum], state_data$Hospital.Name), ]
  
  if( num=="best" ) num <- 1
  if( num=="worst" )  num <- nrow(state_data)
  
  suppressWarnings( rankNum <- as.numeric(num) )
  
  return( state_data[rankNum, ]$Hospital.Name )
}