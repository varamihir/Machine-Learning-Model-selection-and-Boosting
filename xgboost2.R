#XGBoost is the best model in terms of performance, that is to get a good accuracy and execution speed.
##Import the data set
dataset <- read.csv("Churn_Modelling.csv")
dataset <- dataset[,4:14]
# Encoding the categorical variables as factor
dataset$Geography <- as.numeric(factor(dataset$Geography, levels = c("France", "Spain", "Germany"), labels = c(1,2,3)))
dataset$Gender <- as.numeric(factor(dataset$Gender, levels = c("Female", "Male"), labels = c(1,2)))

# Splitting the dataset into training and test set
library(caTools)
split <- sample.split(dataset$Exited, SplitRatio = 0.8)
training_set <- subset(dataset , split == TRUE)
test_set <- subset(dataset, split== FALSE)

# fitting XGBoost to the training set
#install.packages("xgboost")
library(xgboost)
classifier <- xgboost(data = as.matrix(training_set[-11]),label = training_set$Exited, nrounds = 10)
y_pred <- predict(classifier, newdata =as.matrix(test_set[-11]))
#y_pred <- (y_pred >=0.5)
y_pred
cm <- table(test_set[,11], y_pred)
cm
(1534+175)/2000
## Accuracy without K-folds is 85.45%
# Applying K- fold Cross validation
## by creating 10 folds we will eventually get 10 accuracy's
#and 10 accuracy's is the relevant way to measure the accuracy thorugh the mean of these 10 accuracies.
library(caret)
folds <- createFolds(training_set$Exited, k = 10 )
cv <- lapply(folds, function(x){
  training_fold <- training_set[-x,]
  test_fold <- training_set[x,]
  classifier <- xgboost(data = as.matrix(training_set[-11]),label = training_set$Exited, nrounds = 10)
 
  y_pred <- predict(classifier, newdata =as.matrix(test_fold[-11]))
  y_pred <- (y_pred >=0.5)# Convert to binary outcome since XGboost returns probablities
  cm <- table(test_fold[,11], y_pred)
  accuracy <- (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
  return(accuracy)
})

# List of 10 accuracies 
cv
accuracy <- mean(as.numeric(cv))
# Accurcay is 88%
accuracy

