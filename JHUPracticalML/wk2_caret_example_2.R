library(caret); library(kernlab); data(spam)

# Split data into 75% training and 25% testing
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75,
                               list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

# Create 10 folds in data
# Similar approach for createResample and createTimeSlices
set.seed(32323)
folds <- createFolds(y = spam$type,
                     k = 10,
                     list = TRUE,
                     returnTrain = TRUE)
                      #returnTrain = FALSE gives test set folds
sapply(folds, length)

# Basic training
dim(training)
set.seed(32343)
modelFit <- train(type ~.,
                  data = training,
                  method = "glm")

# Resulting model summary
modelFit

# Final fitted values for model
modelFit$finalModel

# Predict classes of testing data
predictions <- predict(modelFit,
                       newdata = testing)
predictions

confusionMatrix(predictions, testing$type)
