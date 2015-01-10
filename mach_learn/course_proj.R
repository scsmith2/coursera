library(caret); library(ggplot2)
# Download test & training csv's
#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","pml-training.csv")
#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","pml-testing.csv")

#Load test data into 'testdata'
td <- read.csv("pml-training.csv",as.is=TRUE)

#Clean up data
names(td)[names(td)=="kurtosis_picth_belt"]<-"kurtosis_pitch_belt"

#Remove columns that are >10% NA
l <- length(td$classe)
td <- td[,colSums(!is.na(td))>(.9*l)]

#Remove columns that are not features
drops <- c("X",
           "user_name",
           "raw_timestamp_part_1",
           "raw_timestamp_part_2",
           "cvtd_timestamp",
           "new_window",
           "num_window")
td <- td[,!(names(td) %in% drops)]

#featurePlot(x = td[,79:86],
            y = td$classe,
            plot = "pairs")

#qplot(td$roll_forearm,td$yaw_forearm,colour=td$classe)

#Convert character features to numeric
#w <- which(sapply(td,class) == 'character')
#td[w] <- lapply(td[w], function(x) as.numeric(x))

dat <- data.frame(td$classe,
                  td$pitch_belt,
                  td$yaw_belt,
                  td$total_accel_belt,
                  td$total_accel_arm,
                  td$total_accel_dumbbell,
                  td$roll_forearm,
                  td$yaw_forearm)

ctrl <- trainControl(method = "cv",
#                     number = 5,
#                     repeats = 10,
                     classProbs = TRUE)

gbmFit <- train(td.classe ~ .,
                data = dat,
                method = "gbm",
                trControl = ctrl,
                metric = "ROC")

ctrl <- trainControl(method = "repeatedcv",
                     number = 5,
                     repeats = 10,
                     classProbs = TRUE)

ldaFit <- train(td$classe ~ .,
                data = td,
                method = "lda",
                trControl = ctrl,
                metric = "ROC")


ldaClasses <- predict(ldaFit,newdata=dat)
confusionMatrix(ldaClasses,dat$td.classe)

nbFit <- train(td.classe ~ .,
               data = dat,
               method="nb")
