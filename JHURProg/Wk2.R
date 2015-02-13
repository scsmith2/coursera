## Wk 3 Lecture Code #######


add2 <- function (x,y) {
  x+y
}

#above() function
above <- function(x,n){
  use <- x > n
  x[use]
}

#mean of columns
columnmean <- function(y, removeNA = TRUE){
  nc <- ncol(y)
  means <- numeric(nc)
  for(i in 1:nc){
    means[i] <- mean(y[,i],na.rm = removeNA)
  }
  means
}


## Wk 3 Homework Code #####
#Q3
{
  f <- function(x){
    g <- function(y){
      y + z
    }
    z <- 4
    x + g(x)
  }
}

#Q4
{
  x <- 5
  y <- if(x < 3) {
    NA
  } else {
    10
  }
}
