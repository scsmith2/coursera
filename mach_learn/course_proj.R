library(caret); library(ggplot2)
# Download test & training csv's
#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv","pml-training.csv")
#download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv","pml-testing.csv")

#Load test data into 'testdata'
td <- read.csv("pml-training.csv")

#Clean up data
names(td)[names(td)=="kurtosis_picth_belt"]<-"kurtosis_pitch_belt"

#Remove columns that are >10% NA
l <- length(td$classe)
td <- td[,colSums(!is.na(td))>(.9*l)]

outcome <- data.frame(td$classe)
predictors <- data.frame(td$roll_belt,
                         td$pitch_belt,
                         td$yaw_belt,
                         td$gyros_belt_x,
                         td$gyros_belt_y,
                         td$roll_arm,
                         td$pitch_arm,
                         td$yaw_arm)

#qplot(td$classe,td$roll_belt,colour=td$user_name)
#qplot(td$classe,td$roll_belt,colour=td$pitch_belt)
#qplot(td$classe,td$roll_arm-td$roll_belt)

fit <- train(