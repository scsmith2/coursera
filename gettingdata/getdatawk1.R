download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv ","acs.csv")
data <- read.csv("acs.csv")
values <- data$VAL
values <- values[!is.na(values)]
above1M <- sum(values == as.integer(24),na.rm=TRUE)
print(above1M)

library(xlsx)
colIndex <- 7:15
rowIndex <- 18:23
dat <- read.xlsx("getdata-data-DATA.gov_NGAP.xlsx",
                   sheetIndex=1,
                   colIndex = colIndex,
                   rowIndex = rowIndex)
sum(dat$Zip*dat$Ext,na.rm=T)

library(XML)
fileURL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileURL,useInternal=TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
stuff <- xmlSApply(rootNode,xmlValue)
library(stringr)
str_count(stuff,"21231")

library(data.table)
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv","acs2.csv")
DT <- fread("acs2.csv")

sapply(split(DT$pwgtp15,DT$SEX),mean)
DT[,mean(pwgtp15),by=SEX]
#rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
tapply(DT$pwgtp15,DT$SEX,mean)
#mean(DT$pwgtp15,by=DT$SEX)
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)










