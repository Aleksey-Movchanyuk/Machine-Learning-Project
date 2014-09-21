library(caret)
library(randomForest)

## Change work directory
setwd("/Users/aleksey/Documents/Coursera/Practical Machine Learning/Course Project")


## Read row data
trainingFile <- 'pml-training.csv'
testingFile     <- 'pml-testing.csv'

training <- read.csv(trainingFile, na.strings = c("", "NA", "#DIV/0!") )
testing <- read.csv(testingFile, na.strings = c("", "NA", "#DIV/0!") )


## Remove 1,5,6 columns 
training       <- training[,-c(1,5,6)]
testing        <- testing[,-c(1,5,6)]


## Define cross validation
## splitting the training data into a test set and a training set
trainingIndex  <- createDataPartition(training$classe, p=.60, list=FALSE)
training.train <- training[ trainingIndex,]
training.test  <- training[-trainingIndex,]


## Remove entire NA columns
rm.na.cols     <- function(x) { x[ , colSums( is.na(x) ) < nrow(x) ] }

training.train <- rm.na.cols(training.train)
training.test  <- rm.na.cols(training.test)


## Removes any variables with missing NAs
complete       <- function(x) {x[,sapply(x, function(y) !any(is.na(y)))] }

training.train <- complete(training.train)
training.test  <- complete(training.test)


## Train a model 
random.forest <- train(training.train[,-57],
                       training.train$classe,
                       tuneGrid=data.frame(mtry=3),
                       trControl=trainControl(method="none")
)


## Print summary statistic
summary(random.forest)


## Build confusion matrix
confusionMatrix(predict(random.forest,
                        newdata=training.test[,-57]),
                training.test$classe
)

## Plot most importance fetures
plot( varImp(random.forest) )