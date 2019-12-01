##########################################
# Data splitting with caret in R
##########################################

# Load the libraries
library(caret)
library(klaR)

# Load the iris dataset
data(iris)
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(iris$Species, p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]

# Train a naive bayes model
model <- NaiveBayes(Species~., data=data_train)
# Make predictions
x_test <- data_test[,1:4]
y_test <- data_test[,5]
predictions <- predict(model, x_test)
# Summarize results
confusionMatrix(predictions$class, y_test)

