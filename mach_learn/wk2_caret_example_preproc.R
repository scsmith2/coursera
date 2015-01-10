library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75,
                               list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
hist(training$capitalAve,
     main = "",
     xlab = "ave. capital run length")

#Noticing that cap. run lengths are all small,
# need to preprocess

mean(training$capitalAve)
sd(training$capitalAve)
trainCapAve <- training$capitalAve
trainCapAveS <- (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAveS)
sd(trainCapAveS)
hist(trainCapAveS,
     main="",
     xlab="ave.capital run length (normalized)")

preObj <- preProcess(training[,-58],method=c("center","scale"))
trainCapAveSt <- predict(preObj,training[,-58])$capitalAve
mean(trainCapAveSt)
sd(trainCapAveSt)

# Box-Cox transform to create 
preObj2 <- preProcess(training[,-58],
                      method = c("BoxCox"))
trainCapAveS3 <- predict(preObj2,
                         training[,-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS3);
qqnorm(trainCapAveS3)

# Imputing data
training$capAve <- training$capitalAve
selectNA <- rbinom(dim(training)[1],size=1,prob=.05)==1
training$capAve[selectNA] <- NA
preObj3 <- preProcess(training[,-58],method="knnImpute")
capAve <- predict(preObj3,training[,-58])$capAve
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

