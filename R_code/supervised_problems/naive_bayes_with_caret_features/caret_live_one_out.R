################################################
# Leave one out cross validation with caret in R
################################################

# load the library
library(caret)
# load the iris dataset
data(iris)

# Define training control
train_control <- trainControl(method="LOOCV")

# Train the model (nb = Naive Bayes)
model <- train(Species~., data=iris, trControl=train_control, method="nb")
# summarize results
print(model)