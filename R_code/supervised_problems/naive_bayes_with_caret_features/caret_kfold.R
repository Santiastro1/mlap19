##########################################
# k-fold Cross Validation with caret in R
##########################################

# Load the library
library(caret)

# Load the iris dataset
data(iris)
# Define training control
train_control <- trainControl(method="cv", number=10)
# Fix the parameters of the algorithm
grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))

# Train the model (nb=Naive Bayes)
model <- train(Species~., data=iris, trControl=train_control, method="nb", tuneGrid=grid)
# Summarize results
print(model)