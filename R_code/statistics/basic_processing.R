##########################################
# Statistical Modelling  #
##########################################

# Libraries
library(ISLR)
library(plot3D)

# ISLR uploads data structure denoted Auto (Information about cars)
# We give it a standard name
data <- Auto


# PRELIMINARY INSPECTION
###########################
# Type of data structure
str(data)

# Names of variables
names(data)

# Properties of data structure
attributes(data)

# Summary of data
summary(data)

# First lines of data structures
head(data,n=5)


# STATISTICAL ANALYSIS OF NUMERICAL VALUES 
############################################
# See columns with numerical values
sapply(data,class)

# Remember alternative way to see such data
str(data)

# For those numerical variables we can compute descriptive statistics.
# Mean value of first column 
mean(data[,1])

# Mean values for 8 first columns (we put 2 to indicate it is by columns), 
# excluding variables with missing values (NA)
apply(data[c(1:8)], 2, mean, na.rm=TRUE) 


# GRAPHICAL REPRESENTATIONS
############################
# Data of one variable
plot(data[,1]) # also plot(datos$mpg)

# Boxplot of one variable
boxplot(data$mpg)

# Histogram
hist(data[,1])

# Relationship between two variables
plot(data$cylinders,data$mpg)

# Fix data to refer to them in a synthetic manner (wihout putting data$ previously)
attach(data)

# Axis x qualitative (factor) allows boxplot on the figure
# Define new qualitative variable 
cylinders_fac <- as.factor(cylinders)
# Relationship between two variables
plot(cylinders_fac,mpg)

# Relationship by pairs of the first three variables
plot(data[c(1:3)])

# Relationship by pairs between all variables
pairs(data)

#SELECTION OR DATA FILTERING
##############################
# Choose cars of a given year
cars_70 <- data[which(data$year==70),]
# Another way to do it
cars_82 <- subset(data, year==82,)


# TESTING HYPOTHESIS (NORMALITY OF DATA)
#################################################
# Probability plot
qqnorm(cars_70$mpg)
qqline(cars_70$mpg)

# Test of normality
shapiro.test(cars_70$mpg)


# HYPOTHESIS TEST: EQUALITY OF MEANS BETWEEN TWO POPULATIONS
################################################################
# Test of equality of miles per galon between those two sets
t.test(cars_70$mpg,cars_82$mpg,conf.level=0.95)


# (COR)RELATIONS BETWEEN VARIABLES
###################################
# We look for relationship between year of car and miles per galon
# Plot one variable versus the other one
plot(weight,mpg)

# Estimate correlation coefficient
cor(weight,mpg)


####################################################################################
# MULTIVARIATE ANALYSIS: DEPENDENCY METHODS: ONE VARIABLE DEPENDS ON OTHER VARIABLES
####################################################################################

######################################
# REGRESSION 
######################################



# LINEAR REGRESSION FOR ONE VARIABLE
####################################
# Adjusting linear model
linear_model <-lm(mpg~weight)

# Content of the model
names(linear_model)

# Confidence interval for the estimated coefficients 
confint(linear_model)

# Summary of the model
summary(linear_model)

# Plot model on the previously plotted data 
abline(linear_model,col="blue")

# Prediction of values with the model: confidence interval
predict(linear_model, data.frame(weight=(c(2000,3000,4000))),interval="confidence")

# Prediction of new values with the model: prediction interval
predict(linear_model, data.frame(weight=(c(2000,3000,4000))),interval="prediction")


# QUALITY MODEL EVALUATION
# Represent 4 graphs of diagnosis of regression in a 2x2 format
par(mfrow=c(2,2))
plot(linear_model)
par(mfrow=c(1,1))

# Plot of residuals (normal and studentized) versus predicted values
plot(predict(linear_model),residuals(linear_model))
plot(predict(linear_model),rstudent(linear_model))

# Statistics of the leverage (distribution of values of independent variable)
plot(hatvalues(linear_model))


# LINEAR REGRESSION WITH SEVERAL VARIABLES
##########################################
# Visualize using already loaded "plot3D" library
scatter3D(weight,horsepower,mpg, xlab="weight", ylab="power", zlab="mpg")

# Adjust with more than one variable
linear_model_several_variables <-lm(mpg~weight+horsepower)

# Summary of model
summary(linear_model_several_variables)

# Factors of variance inflation. Based on library car
library(car)
vif(linear_model_several_variables)

# Regression of all predictors (dot means so) except variable name, which is notnumeric
linear_model_all_numeric_variables <-lm(mpg~.-name,data)

# Summary of the model
summary(linear_model_all_numeric_variables)


# LINEAR MODELS WITH NEW FACTORS
#-------------------------------------
# If the new factors are powers, we can perform a polynomial fitting
# Include interaction terms (crossed products)
modelo_lineal_varias_variables_1 <-lm(mpg~weight+horsepower+weight:horsepower)
# Same command can be writen in synthetic form
modelo_lineal_varias_variables_1 <-lm(mpg~weight*horsepower)

# Include funtions of predictors. Function I() make the symbol ^ to be well interpreted
modelo_lineal_varias_variables_2 <-lm(mpg~weight+I(weight^2))

# If we focus on the use of poluynomials there is a synthetic way to define it
modelo_lineal_varias_variables_5 <-lm(mpg~poly(weight,5))

# Test whose null hypothesis is that both models adjust equally well
anova(modelo_lineal_varias_variables_1,modelo_lineal_varias_variables_2)
anova(modelo_lineal_varias_variables_2,modelo_lineal_varias_variables_5)


######################################################################
# CLASSIFICATION
######################################################################

# Choose data from financial market
datos_cla <- Smarket
names(datos_cla)
dim(datos_cla)
str(datos_cla)
attributes(datos_cla)
summary(datos_cla)

# Choose only complete data (in this case, all)
datos_cla_completos <- na.omit(datos_cla)

attach(datos_cla_completos)
typeof(Direction)
# Exclude non-numeric variable "Direction" for computing correlations
cor(datos_cla_completos[,-9])
plot(Year,Volume)


# MODEL OF LOGISTIC REGRESSION
################################
# For glm to perform logistic -> family= binomial
modelo_logistico <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial)
summary(modelo_logistico)
coef(modelo_logistico)

# Probabilies of prediction for examples from data set 
# See dummmy variable created to caracterize Direction
contrasts(Direction)
probs_predic <- predict(modelo_logistico, type="response")

# Construct a vector for each datum prediction
predic_modelo=rep("Down",length(probs_predic))
predic_modelo[probs_predic>0.5]="Up"

# Confusion matrix
table(predic_modelo,Direction)

# Quality measure (tricky, measure on training set)
mean(predic_modelo==Direction)

# Create a boolean vector called "entrenamiento": it take TRUE value if the 
# corresponding index datum is previous to 2005, and FALSE otherwise
entrenamiento=(Year<2005)

# Such vector allows to choose subsets. 
# For example ejemplo we get data from 2005 o after
# They are NOT from "entrenamiento"
Smarket_2005=Smarket[!entrenamiento,]
Direction_2005=Direction[!entrenamiento]

# Train logistic model only with data from "entrenamiento" (see subset argument)
modelo_logistico2 <- glm(Direction~Lag1+Lag2+Lag3+Lag1+Lag4+Lag5+Volume, family=binomial,subset=entrenamiento)

# We can predict probabilities in the remaining set 
probs_predic2 <- predict(modelo_logistico2, Smarket_2005,type="response")

# Convert probabilistic predictions to up and down values
predic_modelo2=rep("Down",length(probs_predic2))
predic_modelo2[probs_predic2>0.5]="Up"
table(predic_modelo2,Direction_2005)
mean(predic_modelo2==Direction_2005)


# LINEAR DISCRIMINANT ANALYSIS
################################
library(MASS)

# LDA works similar to lm and gm, except it does not have the family option
modelo_lda <- lda(Direction~Lag1+Lag2,subset=entrenamiento)
modelo_lda
plot(modelo_lda)

# Predictions with model (provides a list of three elements)
predic_lda = predict(modelo_lda,Smarket_2005)

# See resulting structure
typeof(predic_lda)
names(predic_lda)
typeof(predic_lda$class)

# Predictions are related to posterior probabilities being >=0.5
# Predictions for 13 first values (n. 12 changes)
predic_lda$class[1:13]
typeof(predic_lda$posterior)
str(predic_lda$posterior)
predic_lda$posterior[1:13,1:2]
sum(predic_lda$posterior[1:13,1]>=0.5)

# Evaluate prediction with first element of list
table(predic_lda$class,Direction_2005)
mean(predic_lda$class==Direction_2005)


# QUADRATIC DISCRIMINANT ANALYSIS
#----------------------------------
# Structure and usage similar to LDA
modelo_qda <- qda(Direction~Lag1+Lag2,subset=entrenamiento)
modelo_qda

# Predictions with model (it provides a three element list)
predic_qda = predict(modelo_qda,Smarket_2005)
table(predic_qda$class,Direction_2005)
mean(predic_qda$class==Direction_2005)



################################################################################
# MULTIVARIATE ANALYSIS: INTERDEPENDENCE METHOS
################################################################################

# PCA
########
estados <- row.names(USArrests)
estados

names(USArrests)
apply(USArrests,2,mean)
apply(USArrests,2,var)

# Apply PCA
resultado_pca = prcomp(USArrests,scale=TRUE)
names(resultado_pca)

# Escaling
resultado_pca$center
resultado_pca$scale

# Transformation (rotation) Matrix of Karhunen Loeve Transform
resultado_pca$rotation

# Vectors of data transformed into component space 
dim(resultado_pca$x)

# Plot first two components
biplot(resultado_pca,scale=0)

resultado_pca$rotation=-resultado_pca$rotation
resultado_pca$x=-resultado_pca$x
biplot(resultado_pca,scale=0)

# Desviations of principal components
resultado_pca$sdev

# Variances of principal components
resultado_pca$var=resultado_pca$sdev^2

# Proportions
prop=resultado_pca$var/sum(resultado_pca$var)
prop

# Plots
plot(prop,xlab="Principal Component", ylab="Proportion of explained variance",ylim=c(0,1),type='b')
plot(cumsum(prop),xlab="Principal Component", ylab="Cumulative proportion of explained variance",ylim=c(0,1),type='b')


###############
# CLUSTERING
###############
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

# K-Means
#--------
# 2 groups; nstart is number of random sets (trials) of initial values of centroids 
resultado_km = kmeans(x,2,nstart=20)

resultado_km$cluster
plot(x,col=(resultado_km$cluste+1),main="Results for K=2",xlab="",ylab="",cex=2)
resultado_km$tot.withinss


