## load packages
library(ggplot2)
library(caret)
library(rpart)
library(randomForest)

## load data
trainurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training <- read.csv(trainurl, na.strings = c(NA, "", "#DIV/0!"))
testing <- read.csv(testurl, na.strings = c(NA, "", "#DIV/0!"))

## preprocessing
## remove the first column (row number)
training <- training[ , -1]
## identify and remove near zero variance predictors
nzv <- nearZeroVar(training)
training <- training[ , -nzv]
## remove predictors with more than 50% NAs 
NAs <- vector(mode = "integer", length = 0L)
for (i in 1 : ncol(training)) {
  if (sum(is.na(training[ , i]))/nrow(training) > 0.5) {
    NAs <- c(NAs, i)
  }
}
training <- training[ , -NAs]
## make test data consistent with training
clean <- colnames(training[ ,-58])
clean <- c(clean, "problem_id")
testing <- testing[ , clean]
## coerce the variables in testing to be same types with training
a <- sapply(training, class)
b <- sapply(testing, class)
a == b ## all TRUE
identical(a, b) ## FALSE: indicating there are factor variables with different levels
for (i in 1:length(testing)) {
  class(testing[ ,i]) <- class(training[ ,i])
}
levels(testing$cvtd_timestamp) <- levels(training$cvtd_timestamp)

## k-fold cross validation
set.seed(103)
k = 10
folds <- createFolds(training$classe, k = k, list = TRUE, returnTrain = FALSE)

## machine learning algorithms
accuracy <- data.frame(rpart = numeric(10), rf = numeric(10))
## predict with dicision tree
for (i in 1:k) {
  subtrain <- training[-folds[[i]], ]
  subtest <- training[folds[[i]], ]
  modfit <- rpart(classe ~ ., data = subtrain, method = "class")
  cmtree <- confusionMatrix(subtest$classe, predict(modfit, subtest, type = "class"))
  accuracy[i,1] <- cmtree$overall[1]
}

## predict with random forest
for (i in 1:k) {
  subtrain <- training[-folds[[i]], ]
  subtest <- training[folds[[i]], ]
  modfit <- randomForest(classe ~ ., data = subtrain)
  cmtree <- confusionMatrix(subtest$classe, predict(modfit, subtest, type = "class"))
  accuracy[i,2] <- cmtree$overall[1]
}
