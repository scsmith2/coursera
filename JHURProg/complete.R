complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  dir <- directory; datasets <- id
    n <- length(datasets)
  soln <- data.frame(matrix(nrow=n,ncol=2))
  names(soln) <- c("id","nobs")
  
  for (i in 1:n){
    j <- formatC(datasets[i],width=3,flag="0")
    x <- read.csv(paste(dir,"/",j,".csv",sep=""))
    x <- sum(complete.cases(x))
#    x <- length(x)
    soln[i,1] = datasets[i]
    soln[i,2] = x
    
  }
  
  soln
  
}