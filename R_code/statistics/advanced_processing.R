#############################################################
# NON-CONVENCIONAL TECHIQUES IN STATISTICS
#############################################################
# Libraries
library(ISLR)
library(plot3D)

# ISLR uploads data structure denoted Auto (Information about cars)
# We give it a standard name
data <- Auto


# K-NEAREST NEIGHBORS (for supervised problems: regression and classification)
#--------------------------------------------------------------------------------------
# Provide 1) matrix of predictors for training, 2) matrix of predictors for test,
# 3) vector of training predicted values (observations), 4) Number of neighbours
library(class)

# Example of use in regression
#-----------------------------
library(FNN)
datos <- Auto
attach(datos)
# Select indexes (row numbers, i.e., integer numbers) of sample 
# Access the complementary set with minus sign
muestra <- sample(1:nrow(datos), 0.9*nrow(datos),replace=FALSE)
datos_entrenamiento <- mpg[muestra]
datos_test <- mpg[-muestra]
datos_entrenamiento_salida <- weight[muestra]
# Execute (insert randomnes to undo ties)
set.seed(3)
# Test data must have matrix format (feature of knn.reg command)
modelo_knn=knn.reg(datos_entrenamiento,as.matrix(datos_test),datos_entrenamiento_salida,k=3)
plot(datos_entrenamiento,datos_entrenamiento_salida)
datos_test_salida <- weight[-muestra]
points(datos_test,modelo_knn$pred, col="blue")
points(datos_test,datos_test_salida,col="red")


# Example of use in classification
#--------------------------------
datos_cla <- Smarket
attach(datos_cla)

# Create a boolean vector called "entrenamiento": it take TRUE value if the 
# corresponding index datum is previous to 2005, and FALSE otherwise
entrenamiento=(Year<2005)

# Such vector allows to choose subsets. 
# For example ejemplo we get data from 2005 o after
# They are NOT from "entrenamiento"
Smarket_2005=Smarket[!entrenamiento,]
Direction_2005=Direction[!entrenamiento]

# Since it is based on logical training, access to complementary with ! before
entrenamiento_X=cbind(Lag1,Lag2)[entrenamiento,]
test_X=cbind(Lag1,Lag2)[!entrenamiento,]
entrenamiento_Direction=Direction[entrenamiento]

# Execute (insert randomnes to undo ties) 
set.seed(11)
predic_knn=knn(entrenamiento_X,test_X,entrenamiento_Direction,k=3)
table(predic_knn,Direction_2005)
mean(predic_knn==Direction_2005)


# DECISION TREES (for supervised problems: both regression and classification)
#--------------------------------------------------------------------------------------
library(tree)

# Prepare data and generate new column of sellings (high=yes if selling>8)
attach(Carseats)
High = ifelse(Sales<=8,"No","Yes")
Carseats = data.frame(Carseats,High)

# Example of use in classification
#--------------------------------
# Try to predic that variable from the rest (except the one employed to generate it)
modelo_arbol = tree(High~.-Sales,Carseats)
summary(modelo_arbol)

# Plot
plot(modelo_arbol)
# pretty=0 es is to include category names
text(modelo_arbol,pretty=0)
modelo_arbol

# Evaluating test error based on predictions (use training and test sets)
set.seed(2)
entrenamiento=sample(1:nrow(Carseats),200)
Carseats_test=Carseats[-entrenamiento,]
High_test=High[-entrenamiento]
modelo_arbol2 = tree(High~.-Sales, Carseats,subset=entrenamiento)
predic_modelo_arbol2=predict(modelo_arbol2,Carseats_test,type="class")
table(predic_modelo_arbol2,High_test)

# Cross validation to determine optimal level of complexity
set.seed(3)
#FUN=prune.misclass for cross validation be guided by classification erro
# If not included, it is guided by "deviance"
valid_cruzada_modelo_arbol=cv.tree(modelo_arbol,FUN=prune.misclass)
names(valid_cruzada_modelo_arbol)
valid_cruzada_modelo_arbol

# We can visualize numer of nodes versus error rate (in this case, that is the meaning of 'dev')
par(mfrow=c(1,2))
plot(valid_cruzada_modelo_arbol$size,valid_cruzada_modelo_arbol$dev,type='b')
# Complexity-cost (k) parameter versus error rate
plot(valid_cruzada_modelo_arbol$k,valid_cruzada_modelo_arbol$dev,type='b')

# We can perform pruning (knowing that size 9 provided lower error rate)
prune_modelo_arbol <- prune.misclass(modelo_arbol,best=9)


# Example in regression
#------------------------------
library(MASS)
set.seed(18)
train =sample(1:nrow(Boston),nrow(Boston)/2)
modelo_arbol_regresion <- tree(medv~.,Boston,subset=train)
summary(modelo_arbol_regresion)

# Plot resulting tree 
plot(modelo_arbol_regresion)
text(modelo_arbol_regresion,pretty=0)

# Cross validation on the modes
validacion_modelo_arbol_regresion <- cv.tree(modelo_arbol_regresion)
plot(validacion_modelo_arbol_regresion$size,validacion_modelo_arbol_regresion$dev,type='b')


# Bagging and Random Forests 
#------------------------------
library(randomForest)
set.seed(12)
modelo_bagging =randomForest(medv~.,data=Boston,subset=entrenamiento,mtry=13,importance=TRUE)
modelo_bagging

# Boosting
#---------
library(gbm)
set.seed(13)
# Since it is a regression problem we put distribucion="gaussian"; 
# For classification we should put distribution="bernoulli
modelo_boosting <- gbm(medv~.,data=Boston[entrenamiento,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(modelo_boosting)


# SUPPORT VECTOR MACHINES (mainly for classification, but also regression
#-------------------------------------------------------------------------
# (Usual) example in classification
#-------------------------------------
set.seed(14)
x=matrix(rnorm(20*2),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,]=x[y==1,]+1

plot(x,col=(3-y))

datos=data.frame(x=x,y=as.factor(y))
library(e1071)

# Model with linear Kernel 
modelo_svm=svm(y~.,data=datos, kernel="linear",cost=10,scale=FALSE)

plot(modelo_svm,datos)
# Support vectors (plotted with crosses)
modelo_svm$index
summary(modelo_svm)

# Another model with less cost (more margin and more support vectors)
modelo_svm2=svm(y~.,data=datos, kernel="linear",cost=0.1,scale=FALSE)

# Command to perform 10-fold cross validation to a collection of obtained models
# obtained with different costs
set.seed(15)
resultado_tune = tune(svm,y~.,data=datos,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1.5,10,100)))

# Cross validation errors
summary(resultado_tune)
mejor_modelo <- resultado_tune$best.model
summary(mejor_modelo)


# SVM WITH NONLINEAR KERNELS 
#----------------------------
# Generate synthetic data (two classes)
set.seed(16)
x = matrix (rnorm(200*2),ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
datos <- data.frame(x=x,y=as.factor(y))
plot(x,col=y)

# Separate training set 
entrenamiento=sample(200,100)
# Fit radial kernel model 
modelo_svm_rad <- svm(y~.,data=datos[entrenamiento,], kernel="radial", gamma=1, cost=1)
# Visualize results
plot(modelo_svm_rad, datos[entrenamiento,])
summary(modelo_svm_rad)

# SVM FOR SEVERAL CLASSES
#-----------------------
set.seed(17)
# Use previous data
x = rbind(x,matrix (rnorm(50*2),ncol=2))
y=c(y,rep(0,50))
x[y==0,2]=x[y==0,2]+2
datos = data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

# Model
modelo_svm_rad2 <- svm(y~.,data=datos, kernel="radial", gamma=1, cost=10)
# Visualize results
plot(modelo_svm_rad2, datos)


########################################
# UNSUPERVISED PROBLEM TECHNIQUES
########################################


# CLUSTERING
###############
# Hierarchical
#--------------
resultado_hc_complete = hclust(dist(x),method="complete")
plot(resultado_hc_complete,main="Linkaje completo",xlab="",sub="",cex=0.9)
cutree(resultado_hc_complete,2)
library(gplots)
heatmap.2(data.matrix(x),main="Hierarchical Cluster", Rowv=as.dendrogram(resultado_hc_complete), 
          Colv=NA,dendrogram="row", scale="row",density.info="none",trace="none")



# SELF ORGANIZING MAPS (SOM) (Comparing with K-means)
#####################################################

# Generate 5 different samples (each of size 20) from
# 5 independet bivariate Gaussian distribution 
# Define mean vector and covariance matrix
mu1<- c(0,0)
Sigma1 <- matrix(c(1,0.5,0.5,1),byrow=T,nrow=2)
g1 <- mvrnorm(n=20,mu1,Sigma1)

mu2<- c(4,0)
Sigma2 <- matrix(c(1,0.3,0.3,1),byrow=T,nrow=2)
g2 <- mvrnorm(n=20,mu2,Sigma2)

mu3<- c(0,4)
Sigma3 <- matrix(c(1,0.8,0.8,1),byrow=T,nrow=2)
g3 <- mvrnorm(n=20,mu3,Sigma3)

mu4<- c(10,10)
Sigma4 <- matrix(c(1,0.1,0.1,1),byrow=T,nrow=2)
g4 <- mvrnorm(n=200,mu4,Sigma4)

mu5<- c(0,10)
Sigma5 <- matrix(c(1,0.1,0.1,1),byrow=T,nrow=2)
g5 <- mvrnorm(n=200,mu5,Sigma5)

# Gather all data in a matrix
g <- rbind(g1,g2,g3,g4,g5)

# Plot them
plot(g)

# Analysis via K-means for comparison with SOM
library(cluster)
wss <- (nrow(g)-1)*sum(apply(g,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(g,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of 
squares") 

# Análisis Cluster K-Means: K chosen depending on the "elbow" in the graph
km.out <- kmeans(g, 5) # solution with 5 clusters
# Obtain average values of each cluster
centroids <- aggregate(g,by=list(km.out$cluster),FUN=mean)
print(centroids)
plot(centroids$V1,centroids$V2)
# Add cluster assignment to data
g_con_cluster <- data.frame(g, km.out$cluster) 

# Plot data projections
# vary parameters for a better graph reading
clusplot(g, km.out$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)



# SOM
######
library(som)
# In this bivariate example SOM does not provide dimensionality reduction
salida_som <- som(g,xdim=3,ydim=3,alphaType="inverse",neig="gaussian",topol="hexa",
               init="sample",radius=c(3,1),rlen=c(100,1000))
plot(salida_som)
# Code vector xdim*ydim rowwise rownumber=x+y*xdim
print(salida_som$code)
plot(salida_som$code)

