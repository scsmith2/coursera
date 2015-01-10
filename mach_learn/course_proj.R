library(caret); library(ggplot2)
# Download test & training csv's
#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","pml-training.csv")
#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","pml-testing.csv")

#Load test data into 'testdata'
training <- read.csv("pml-training.csv",as.is=TRUE)
testing <- read.csv("pml-testing.csv",as.is=TRUE)
#Clean up data
names(training)[names(training)=="kurtosis_picth_belt"]<-"kurtosis_pitch_belt"
names(testing)[names(testing)=="kurtosis_picth_belt"]<-"kurtosis_pitch_belt"

trn <- data.frame(training$classe,
                  training$pitch_belt,
                  training$yaw_belt,
                  training$total_accel_belt,
                  training$total_accel_arm,
                  training$total_accel_dumbbell,
                  training$roll_forearm,
                  training$yaw_forearm)
colnames(trn)<-c("classe","pitch_belt","yaw_belt","total_accel_belt",
                "total_accel_arm","total_accel_dumbbell","roll_forearm","yaw_forearm")

tst <- data.frame(#testing$classe,
                  testing$pitch_belt,
                  testing$yaw_belt,
                  testing$total_accel_belt,
                  testing$total_accel_arm,
                  testing$total_accel_dumbbell,
                  testing$roll_forearm,
                  testing$yaw_forearm)
colnames(tst)<-c("pitch_belt","yaw_belt","total_accel_belt",
                "total_accel_arm","total_accel_dumbbell","roll_forearm","yaw_forearm")
gbmCtrl <- trainControl(method = "cv",
                        number = 10,
                        classProbs = TRUE)
set.seed(137)
gbmFit <- train(classe ~ .,
                data = trn,
                method = "gbm",
                trControl = gbmCtrl,
                metric = "ROC")

ggplot(gbmFit)

gbmTrnClasses <- predict(gbmFit,newdata=trn)
gbmTrnProbs <- predict(gbmFit,newdata=trn,type="prob")
confusionMatrix(gbmTrnClasses,trn$classe)

gbmTstClasses <- predict(gbmFit,newdata=tst)
gbmTstProbs <- predict(gbmFit,newdata=tst,type="prob")
print(gbmTstClasses)
print(gbmTstProbs)
