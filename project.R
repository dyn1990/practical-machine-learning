library(knitr)
library(caret)
library(randomForest)
library(e1071)
library(dplyr)
library(ggplot2)
library(ggcorrplot)



trainFile <- "pml-training.csv"
testFile <- "pml-testing.csv"
if (!file.exists(trainFile)){
        trainURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        download.file(trainURL, destfile = trainFile)
}
if (!file.exists(testFile)){
        testURL  <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(trainURL, destfile = testFile)
}

train <- read.csv(trainFile)
test <- read.csv(testFile)


## remove identification variables which don't contribute to statistics
train <-  train[, -c(1, 2)]
test <-  test[, -c(1, 2)]

## remove time variables which are not related to the goal of this project.
time_index <- grep("time", names(train))
train <- train[, -time_index]
test <- test[, -time_index]

## remove most NA variables
NA_index <- sapply(train, function(x) {mean(!is.na(x))}) > 0.95
train <- train[, NA_index == TRUE]
test <- test[, NA_index == TRUE]

## remove near zero variance variables
NZV <- nearZeroVar(train)
train <- train[, -NZV]
test <- test[, -NZV]


inTrain <- createDataPartition(y = train$classe, p = 0.8, list = FALSE)
trainSet <- train[inTrain, ]
testSet <- train[-inTrain, ]
print(data.frame("trainDim" = dim(trainSet), "testDim" = dim(testSet)))

# corr <- round(cor(trainSet[,-54]), 1)
# g <- ggcorrplot(corr, hc.order = TRUE, 
#            type = "lower", tl.cex = 7, tl.srt = 70,
#            title="Correlogram of Activity")
# g


### model
# random forest
set.seed(1111)
fit_rf <- randomForest(classe ~ ., data = trainSet)
print(fit_rf$confusion)
pred_rf <- predict(fit_rf, newdata = testSet)
print(confusionMatrix(pred_rf, testSet$classe)$table)
# 
# ## support vector machine
# set.seed(1111)
# fit_svm <- svm(classe ~ ., data = trainSet)
# print(table(predict(fit_svm, trainSet[, -54]), trainSet[, 54]))
# print(table(predict(fit_svm, testSet[, -54]), testSet[, 54]))

## generalized boosted model
# control_gbm <- trainControl(method = "repeatedcv", number = 2, repeats = 1)
# fit_gbm  <- train(classe ~ ., data=trainSet, method = "gbm",
#                   trControl = control_gbm, verbose = FALSE)
# print(confusionMatrix(predict(fit_gbm, newdata = trainSet), trainSet$classe)$table)
# pred_gbm <- predict(fit_gbm, newdata = testSet)
# print(confusionMatrix(pred_gbm, testSet$classe)$table)

