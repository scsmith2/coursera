library(caret); library(kernlab); data(spam)
inTrain <- createDataPartition(y = spam$type,
                               p = 0.75,
                               list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
set.seed(23343)
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
