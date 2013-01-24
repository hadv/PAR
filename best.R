best <- function(state, outcome)
{
  # Read data
  data <-read.csv("./ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  
  # Convert character into numeric
  suppressWarnings(data[, 11] <- as.numeric(data[, 11]) )
  suppressWarnings(data[, 17] <- as.numeric(data[, 17]) )
  suppressWarnings(data[, 23] <- as.numeric(data[, 23]) )
  
  table <- data.frame(State=names(tapply(data$State, data$State, length)), 
                      Freq=tapply(data$State, data$State, length))
  rownames(table) <- NULL
  
  input <- data.frame(Outcome=c("heart attack", "heart failure","pneumonia"), Col=c(11, 17, 23))
  
  # Check that state and outcome are valid or not?
  if( nrow(table[table$State==state,]) == 0 ) stop( "invalid state" )
  if( nrow(input[input$Outcome==outcome,]) == 0 ) stop( "invalid outcome" )
  
  stateV <- data[data$State==state, ]
  colNum <- input[input$Outcome==outcome, 2]
  rowNum <- which.min(stateV[, colNum])
  return( stateV[rowNum, ]$Hospital.Name )
}