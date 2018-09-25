# K-Folds cross validation
# Will use the same template from SVM model
# Importing the dataset
dataset = read.csv('Social_Network_Ads.csv')
dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])
# Fitting classifier to the Training set
library(e1071)
classifier <- svm(formula = Purchased ~., data = training_set,
                  type = 'C-classification',
                  kernel = 'radial')
# Predicting the test set
y_pred <- predict(classifier, newdata = test_set[-3])
# Confusion matrix
cm <- table(test_set[,3], y_pred)
cm
#install.packages("caret)
library(caret)
# Applying K- fold Cross validation
## by creating 10 folds we will eventually get 10 accuracy's
#and 10 accuracy's is the relevant way to measure the accuracy thorugh the mean of these 10 accuracies.
folds <- createFolds(training_set$Purchased, k = 10 )
folds
cv <- lapply(folds, function(x){
  training_fold <- training_set[-x,]
  test_fold <- training_set[x,]
  classifier <- svm(formula = Purchased ~., 
                    data = training_fold,
                    type = 'C-classification',
                    kernel = 'radial')
  y_pred <- predict(classifier, newdata = test_fold[-3])
  cm <- table(test_fold[,3], y_pred)
  accuracy <- (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
  return(accuracy)
})
# List of 10 accuracies 
cv
# Relevant accuracy of this model it is 91.34%  
accuracy <- mean(as.numeric(cv))
accuracy
