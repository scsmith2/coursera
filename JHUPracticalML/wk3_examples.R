# Random Forest
{
data(iris); library(ggplot2); library(caret)
set.seed(1033)
inTrain <- createDataPartition(y = iris$Species,
                               p = 0.7,
                               list = FALSE)
training <- iris[inTrain,]
testing <- iris[inTest,]

modFit <- train(Species ~ .,
                data = training,
                method = "rf",
                prox = TRUE)
modFit
getTree(modFit$finalModel, k = 2) #for 2nd tree
irisP <- classCenter(training[,c(3,4)],
                     training$Species,
                     modFit$finalModel$prox)
irisP <- as.data.frame(irisP)
irisP$Species <- rownames(irisP)
p <- qplot(Petal.Width, Petal.Length,
           col = Species, data = training)
p <- geom_point(aes(x = Petal.Width,
                   y = Petal.Length,
                   col = Species),
               size = 5, shape = 4, data = IrisP)
}

# Boosting
{
library(ISLR); data(Wage)
library(ggplot2); library(caret)
Wage <- subset(Wage, select = -c(logwage))
inTrain <- createDataPartition(y = Wage$wage,
                               p = .7, list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

modFit <- train(wage ~ .,
                method = "gbm",
                data = training,
                verbose = FALSE)
print(modFit)
}

# Quiz 3 - Question 1
{
  library(AppliedPredictiveModeling); library(caret)
  data(segmentationOriginal)
  training <- segmentationOriginal[segmentationOriginal$Case == "Train",]
  testing <- segmentationOriginal[segmentationOriginal$Case == "Test",]
  set.seed(125)
  rpartFit <- train(Class ~ .,
                    method = "rpart",
                    data = training)
  
  test <- testing[1:4,]
  test[,] <- is.na(test[,])

  test$TotalIntenCh2 <- c(23000,50000,57000,NA)
  test$FiberWidthCh1 = c(10,10,8,8)
  test$VarIntenCh4 = c(NA,100,100,100)
  test$PerimStatusCh1 = c(2,NA,NA,2)
  pred <- predict(rpartFit,test,type="prob",na.action=na.omit)
}

# Quiz 3 - Question 3
{
  library(pgmm); library(caret);
  data(olive)
  olive = olive[,-1]
  modFit <- train(Area ~ .,data = olive,method = "tree")
  newdata <- as.data.frame(t(colMeans(olive)))
  pred <- predict(modFit,newdata,method="class")
  # Problem is that numeric is returned, should convert
  # 'Area' column to factor before training.
}

# Quiz 3 - Question 4
{
  library(ElemStatLearn); library(caret)
  data(SAheart)
  #SAH <- SAheart[,c(10,9,8,7,2,6,3)]
  set.seed(8484)
  train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
  trainSA = SAheart[train,]
  testSA = SAheart[-train,]
  set.seed(13234)
  glmFit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
                  method = "glm",
                  family = "binomial",
                  data = trainSA)
  predTrain <- predict(glmFit,trainSA)
  predTest <- predict(glmFit,testSA)
  
  missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
  
  trMissClass <- missClass(trainSA$chd,predTrain)
  teMissClass <- missClass(testSA$chd,predTest)
}

#Quiz 3 - Question 5
{
  library(ElemStatLearn); library(caret); library(randomForest)
  data(vowel.train); data(vowel.test)
  vowel.train$y <- as.factor(vowel.train$y)
  vowel.test$y <- as.factor(vowel.test$y)
  set.seed(33833)
  rfFit <- randomForest(y ~ ., data = vowel.train)
  varImp(rfFit)
  
  
  
  