# Question 1
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(url,"ACSW3.csv")
datACS <- read.csv("ACSW3.csv",as.is=TRUE)
# $ACR = 3 .House on ten or more acres
# $AGS = 6 .$10000+ Sales of Agriculture Products
datACS$agricultureLogical = FALSE
datACS$agricultureLogical[datACS$ACR==3&datACS$AGS==6] <- TRUE
head(datACS$agricultureLogical,100)
which(datACS$agricultureLogical)

# Question 2
library(jpeg)
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
download.file(url,"jeff.jpg",mode="wb")
jeffPic <- readJPEG("jeff.jpg",native=TRUE)
quantile(jeffPic,probs=c(.3,.8))
