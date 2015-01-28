#Regularized Regression
library(ElemStatLearn); data(prostate)
str(prostate)
small=prostate[1:5,]
lm(lpsa ~ ., data = small) #Fit linear model


#Combining Predictors
library(ISLR); data(Wage);
library(ggplot2); library(caret)
Wage <- subset(Wage, select = -c(logwage))
inBuild <- createDataPartition(y = Wage$wage,
                               p = .7, list = FALSE)
validation <- Wage[-inBuild,]
buildData <- Wage[inBuild,]
inTrain <- createDataPartition(y = buildData$wage,
                               p = .7, list = FALSE)
training <- buildData[inTrain,]
testing <- buildData[-inTrain,]
dim(training); dim(testing); dim(validation)

mod1 <- train(wage ~ .,
              method = "glm",
              data = training)
mod2 <- train(wage ~ .,
              method = "rf",
              data = training,
              trControl = trainControl(method = "cv",
                                       number = 3))
pred1 <- predict(mod1, testing)
pred2 <- predict(mod2, testing)
qplot(pred1, pred2, color = wage, data = testing)

predDF <- data.frame(pred1,pred2,wage=testing$wage)
combModFit <- train(wage ~ .,
                    method = "gam",
                    data = predDF)
combPred <- predict(combModFit, predDF)
RMSE1 <- sqrt(sum((pred1-testing$wage)^2))
RMSE2 <- sqrt(sum((pred2-testing$wage)^2))
RMSEC <- sqrt(sum((combPred-testing$wage)^2))
print(c(RMSE1,RMSE2,RMSEC)) #Comparing errors
#Check against validation set
pred1V <- predict(mod1,validation)
pred2V <- predict(mod2,validation)
predVDF <- data.frame(pred1=pred1V,pred2=pred2V)
combPredV <- predict(combModFit,predVDF)
RMSEV1 <- sqrt(sum((pred1V-validation$wage)^2))
RMSEV2 <- sqrt(sum((pred2V-validation$wage)^2))
RMSEVC <- sqrt(sum((combPredV-validation$wage)^2))
print(c(RMSEV1,RMSEV2,RMSEVC))

#Forecasting
library(quantmod); library(forecast)
from.dat <- as.Date("01/01/08", format="%m/%d/%y")
to.dat <- as.Date("12/31/13", format="%m/%d/%y")
getSymbols("DAN",src="google",from=from.dat, to=to.dat)
head(DAN)
mDAN <- to.monthly(DAN)
DANOpen <- Op(mDAN)
ts1 <- ts(DANOpen, frequency = 12)
plot(ts1, xlab = "Years+1", ylab = "DAN")
#Can decompose into trends,seasonal patterns,cycles
plot(decompose(ts1),xlab="Years+1")

ts1Train <- window(ts1,start=1,end=5)
ts1Test <- window(ts1,start=5,end=7-.01)
ts1Train; ts1Test
plot(ts1Train)
lines(ma(ts1Train,order=3),col="red")

ets1 <- ets(ts1Train,model="MMM")
fcast <- forecast(ets1)
plot(fcast); lines(ts1Test,col="red")

accuracy(fcast,ts1Test)

#Unsupervised Learning
data(iris); library(ggplot2)
inTrain <- createDataPartition(y = iris$Species,
                               p = .7, list = FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
dim(training); dim(testing)

kMeans1 <- kmeans(subset(training,
                         select = -c(Species)),
                  centers = 3)
training$clusters <- as.factor(kMeans1$cluster)
qplot(Petal.Width,Petal.Length,colour=clusters,data=training)
table(kMeans1$cluster,training$Species)
modFit <- train(clusters ~ .,
                data = subset(training,
                              select = -c(Species)),
                method = "rpart")
table(predict(modFit, training), training$Species)

testClusterPred <- predict(modFit,testing)
table(testClusterPred,testing$Species)

# Quiz 4 - Question 1
library(ElemStatLearn)
data(vowel.train); data(vowel.test)
training <- vowel.train
testing <- vowel.test
training$y <- as.factor(training$y)
testing$y <- as.factor(testing$y)
set.seed(33833)
rfFit <- train(y ~ .,
               method = "rf",
               data = training)
gbmFit <- train(y ~ .,
                method = "gbm",
                data = training)
rfPred <- predict(rfFit,testing)
gbmPred <- predict(gbmFit,testing)
rfAcc <- sum(testing$y==rfPred)/length(rfPred)
gbmAcc <- sum(testing$y==gbmPred)/length(gbmPred)
combAcc <- sum((testing$y==gbmPred)&(testing$y==rfPred))/sum(gbmPred==rfPred)
print(c(rfAcc,gbmAcc,combAcc))

#Quiz 4 - Question 2
library(caret); library(gbm)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
set.seed(3433)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis,p = .75, list=FALSE)
training = adData[inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
rfFit <- train(diagnosis ~ .,
               method = "rf",
               data = training)
gbmFit <- train(diagnosis ~ .,
                method = "gbm",
                data = training)
ldaFit <- train(diagnosis ~ .,
                method = "lda",
                data = training)
rfPred <- predict(rfFit,testing)
gbmPred <- predict(gbmFit,testing)
ldaPred <- predict(ldaFit,testing)
comb <- data.frame(rfPred,
                   gbmPred,
                   ldaPred,
                   diagnosis = testing$diagnosis)
combMod <- train(diagnosis ~ .,
                 method = "rf",
                 data = comb)
combPred <- predict(combMod,testing)
rfAcc <- sum(rfPred==testing$diagnosis)/length(rfPred)
gbmAcc <- sum(gbmPred==testing$diagnosis)/length(gbmPred)
ldaAcc <- sum(ldaPred==testing$diagnosis)/length(ldaPred)
combAcc <- sum(combPred==testing$diagnosis)/length(combPred)
print(c("rfAcc",rfAcc,
        "gbmAcc",gbmAcc,
        "ldaAcc",ldaAcc,
        "combAcc",combAcc))

# Quiz 4 - Question 3
library(AppliedPredictiveModeling); library(caret)
data(concrete)
set.seed(3523)
inTrain = createDataPartition(concrete$CompressiveStrength,
                              p = 3/4)[[1]]
training = concrete[inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
lassoFit <- train(CompressiveStrength ~ .,
                  method = "lasso",
                  data = training)
plot.enet(lassoFit$finalModel,xvar="penalty",use.color=TRUE)

# Quiz 4 - Question 4
library(lubridate)  # For year() function below
library(forecast)
url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
download.file(url,"gaData.csv")
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

fit <- bats(tstrain)
# check how long the test set is, so you can predict beyond trainign
h <- dim(testing)[1]

...

# Quiz 4 - Question 5
set.seed(3523)
library(AppliedPredictiveModeling); library(e1071)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
svmFit <- svm(CompressiveStrength ~ ., data = training)
svmPred <- predict(svmFit,testing)
svmRMSE <- sqrt(sum((svmPred-testing$CompressiveStrength)^2)/length(svmPred))





