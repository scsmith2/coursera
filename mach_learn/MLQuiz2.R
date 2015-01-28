print("Question 1"){
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

set.seed(311)
adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]
}

print("Question 2"){
  library(AppliedPredictiveModeling); library(Hmisc)
  data(concrete)
  library(caret)
  set.seed(975)
  inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
  training = mixtures[ inTrain,]
  testing = mixtures[-inTrain,]
  index <- 1:nrow(training)
  qplot(index,training$CompressiveStrength,color=training$BlastFurnaceSlag)
  qplot(index,training$CompressiveStrength,color=training$FlyAsh)
  qplot(index,training$CompressiveStrength,color=training$Water)
  qplot(index,training$CompressiveStrength,color=training$Superplasticizer)
  qplot(index,training$CompressiveStrength,color=training$CoarseAggregate)
  qplot(index,training$CompressiveStrength,color=training$FineAggregate)
  qplot(index,training$CompressiveStrength,color=training$Age)
  print("variable missing")
}

print("Question 3"){
  library(AppliedPredictiveModeling)
  data(concrete)
  library(caret)
  set.seed(975)
  inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
  training = mixtures[ inTrain,]
  testing = mixtures[-inTrain,]

print("Question 4"){
  library(caret)
  library(AppliedPredictiveModeling)
  set.seed(3433)
  data(AlzheimerDisease)
  adData = data.frame(diagnosis,predictors)
  inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
  training = adData[ inTrain,]
  testing = adData[-inTrain,]
  ILs <- which(substr(colnames(training),1,2)=="IL")
  preProc <- preProcess(training[,ILs],method="pca",thresh=.8)
}

print("Question 5"){
  library(caret)
  library(AppliedPredictiveModeling)
  set.seed(3433)
  data(AlzheimerDisease)
  adData = data.frame(diagnosis,predictors)
  inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
  training = adData[ inTrain,]
  testing = adData[-inTrain,]
  ILs <- which(substr(colnames(training),1,2)=="IL")
  preProc <- preProcess(training[,ILs],method="pca",thresh=.8)
  trainPC <- predict(preProc,training[,-1])
  glmFit_noPCA <- train(diagnosis ~ .,
                        method = "glm",
                        data = training)
  confusionMatrix(testing$diagnosis,predict(glmFit_noPCA,testing))
  glmFit_PCA <- train(training$diagnosis ~ .,
                      method = "glm",
                      preProcess = preProc,
                      data = training)
  
  
  library(caret)
  library(AppliedPredictiveModeling)
  set.seed(3433)
  data(AlzheimerDisease)
  adData = data.frame(diagnosis,predictors)
  inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
  training = adData[ inTrain,]
  testing = adData[-inTrain,]
  ss <- testing[,grep('^IL', x = names(testing) )]
  model1 <- train(ss, testing$diagnosis, method='glm')
  model2 <- preProcess(ss, method='pca', thresh = 0.8, outcome = testing$diagnosis)
  