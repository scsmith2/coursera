

rankall <- function(outcome,num="best") {
  data <- read.csv("outcome-of-care-measures.csv",
                   colClasses = "character")
  source("rankhospital.R")
  state <- unique(data$State)
  state <- state [order(state)]
  oc <- outcome
  n <- num
  hospital <- as.data.frame(sapply(state,rankhospital,outcome=oc,num=n))
  df <- data.frame(hospital,state)
  colnames(df) <- c("hospital","state")
  return(df)
}