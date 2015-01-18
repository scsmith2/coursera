Question 2 {
  library(sqldf)
  acs <- read.csv("getdata-data-ss06pid.csv")
  sum(acs$PWGTP1[acs$AGEP<50])
  sum(sqldf("select pwgtp1 from acs where AGEP < 50"))
}

Question 3 {
  sqldf("select distinct AGEP from acs")
  unique(acs$AGEP)
}

Question 4{
  url <- "http://biostat.jhsph.edu/~jleek/contact.html"
  htmlCode <- readLines(url)
  n10 <- nchar(htmlCode[10])
  n20 <- nchar(htmlCode[20])
  n30 <- nchar(htmlCode[30])
  n100 <- nchar(htmlCode[100])
  paste(n10,n20,n30,n100)
}

Question 5{
  cols = c(1,9,5,4,4,5,4,4,5,4,4,5,4,4)
  a5 <- read.fwf("getdata-wksst8110.for",cols,as.is=TRUE)
  dat <- data.frame(a5$V2,a5$V4,a5$V5,a5$V7,a5$V8,a5$V10,a5$V11,a5$V13,a5$V14)
  dat <- dat[-c(1:4),]
  dat <- as.data.frame(apply(dat,2,function(x)gsub('\\s+', '',x)),colClasses="character")
  sum4 <- sum(as.numeric(as.character(dat$a5.V7)))
  print(sum4)
