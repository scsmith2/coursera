library(caret)
library(mlbench)
data(Sonar)
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class,
                               ## the outcome data are needed
                               p = .75,
                               ## the percentage of data
                               ## in the training set
                               list = FALSE)
                               ## the results format
## output for inTrain is set of integers for rows
## of sonar that belong in the training set
training <- Sonar[inTrain,]
testing <- Sonar[-inTrain,]
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)
plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                tuneLength = 15,
                trControl = ctrl,
                metric = "ROC",
                preProc = c("center", "scale"))

# Generate a set of 
plsClasses <- predict(plsFit, newdata = testing)
plsProbs <- predict(plsFit, newdata = testing, type = "prob")
confusionMatrix(data = plsClasses, testing$Class)

rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit <- train(Class ~ .,
                data = training,
                method = "rda",
                tuneGrid = rdaGrid,
                trControl = ctrl,
                metric = "ROC")
rdaFit
rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)

# Compare resampling results
resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)

# Plot ROC
library(pROC)
plsROC <- roc(testing$Class, plsClasses,
              positive=levels(plsClasses)[1])
