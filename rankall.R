rankall <- function(outcome, num = "best")
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
  
  if( nrow(input_data[input_data$Outcome==outcome,]) == 0 ) stop( "invalid outcome" )
  
  ## For each state, find the hospital of the given rank
  hospitalsName <- character(0)
  for(state in table_data$State)
  {
    state_data <- outcome_data[outcome_data$State==state, ]
    colNum <- input_data[input_data$Outcome==outcome, 2]
    state_data <- state_data[complete.cases(state_data[, colNum]), ]
    state_data <- state_data[order(state_data[, colNum], state_data$Hospital.Name), ]
    
    if( num=="best" )   rankNum <- 1
    else if( num=="worst" )  rankNum <- nrow(state_data)
    else suppressWarnings( rankNum <- as.numeric(num) )
    
    hospitalsName <- c( hospitalsName, state_data[rankNum, ]$Hospital.Name )
  }
  
  return( data.frame(hospital=hospitalsName, state=table_data$State) )
}