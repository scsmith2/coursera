library(caret); library(kernlab); data(spam)

rule2 <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x > 2.40] <- "spam"
  prediction[x < 2.40] <- "nonspam"
  return(prediction)
}

spamNA <- sum(is.na(rule2(spam$capitalAve)))
print(paste0("There are ",spamNA," NAs in the returned character vector"))
print("Therefore the 'na.rm = TRUE' argument is needed in 'sum()' and 'mean()'")

sumClasses <- sum(rule2(spam$capitalAve)==spam$type,na.rm=TRUE)
meanClasses <- mean(rule2(spam$capitalAve)==spam$type,na.rm=TRUE)
print(paste0("The prediction matched ",
             sumClasses,
             " of the ",
             length(spam$type)-spamNA,
             " valid data points",
             " for an accuracy of ",
             round(100*meanClasses,1),
             "%."))

sum(spam$capitalAve==2.4)
