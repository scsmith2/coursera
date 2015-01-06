best <- function (state,outcome) {
  data <- read.csv("outcome-of-care-measures.csv",
                    colClasses = "character")

  # Check state validity
  states <- unique(data$State)
  if (state %in% states) {
  } else {
    stop("invalid state")
  }
  
  # Lookup outcome
  if (outcome == "heart attack") {
#    outname <- Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
    c <- 11
  } else if (outcome == "heart failure") {
#    outname <- Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
    c <- 17
  } else if (outcome == "pneumonia") {
#    outname <- Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
    c <- 23
  } else {
    stop("invalid outcome")
  }

newdata <- subset(data,data$State==state)
besthosp <- newdata$Hospital.Name[which.min(newdata[,c])]
return(besthosp)
}

