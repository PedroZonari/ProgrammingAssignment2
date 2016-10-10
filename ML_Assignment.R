library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)
library(repmis)

# Importing data
training<-read.csv("pml-training.csv",na.strings=c("NA",""))
testing<-read.csv("pml-testing.csv",na.strings=c("NA",""))

# Cleaning data (removing meaningless columns)
training<-training[,colSums(is.na(training))==0]
testing<-testing[,colSums(is.na(testing))==0]

trainData <- training[, -c(1:7)]
testData <- testing[, -c(1:7)]

# Spliting data 
set.seed(1000) 
inTrain <- createDataPartition(trainData$classe, p = 0.7, list = FALSE)
train <- trainData[inTrain, ]
valid <- trainData[-inTrain, ]

## Random forest 
control <- trainControl(method = "cv", number = 4)
fit_rf <- train(classe ~ ., data = train, method = "rf", 
                trControl = control)
print(fit_rf, digits = 4)

predict_rf <- predict(fit_rf, valid)
conf_rf <- confusionMatrix(valid$classe, predict_rf)

accuracy_rf <- conf_rf$overall[1]

# Predicting on test data
predict(fit_rf, testData)
