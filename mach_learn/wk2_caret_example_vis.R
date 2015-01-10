library(ISLR); library(ggplot2); library(caret);
data(Wage)
summary(Wage)

inTrain <- createDataPartition(y = Wage$wage,
                               p = 0.7,
                               list = FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]
dim(training); dim(testing)

# Plot relationships between features and outcomes
featurePlot(x = training[,c("age","education","jobclass")],
            y = training$wage,
            plot = "pairs")

# Scatterplots of data
qplot(age,wage,data = training)
qplot(age,wage,data = training, colour = jobclass)

# Adding regression smoothers
qq <- qplot(age,wage,colour = education, data = training)
qq + geom_smooth(method = 'lm', formula = y~x)

# Cut into categories
library(Hmisc)
cutWage <- cut2(training$wage, g = 3)
table(cutWage)

#Plot groups vs age
library(gridExtra) #needed for grid.arrange()
p1 <- qplot(cutWage,age,data=training, fill=cutWage,
            geom = c("boxplot"))
p1

p2 <- qplot(cutWage,age,data=training, fill=cutWage,
            geom = c("boxplot","jitter"))
grid.arrange(p1,p2,ncol=2)

t1 <- table(cutWage,training$jobclass)

prop.table(t1,1)
t1

# Density plot
qplot(wage,colour=education, data = training, geom = "density")

