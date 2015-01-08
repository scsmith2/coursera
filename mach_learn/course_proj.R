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

#Convert character features to numeric
#w <- which(sapply(td,class) == 'character')
#td[w] <- lapply(td[w], function(x) as.numeric(x))

#Transform NAs in converted columns to 0
#td[is.na(td)] <- 0


ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     classProbs = TRUE)
adaFit <- train(classe ~ .,
                data = td,
                method = "ada",
                trControl = ctrl,
                metric = "ROC")

# Generate a set of 
plsClasses <- predict(plsFit, newdata = testing)
plsProbs <- predict(plsFit, newdata = testing, type = "prob")