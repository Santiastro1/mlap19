################################################################
# REGRESSION WITH DIFFERENT ARCHITECTURES
################################################################
# Clean environment
rm(list=ls())


# Define sample points (example in slides)
n <- 10
x <- c(0.6737722, 0.1148212, 0.9783095, 0.7210195, 0.2608685, 0.8137591, 0.1468300, 0.5760339, 0.5921425, 0.1599696)
y <- c(0.42990423, 0.20180591, 1.18422641, 0.66459027, 0.36460305, 0.92360603, 0.17801866, 0.70802263, 0.64622718, 0.09985043)


## CONSTRUCT ANOTHER DATA SET BASED ON A DIFFERENT MODEL
## We can generate points along any function to be defined (plus noise)
## Number of points
#n <- 10
## Fix seed for replication purposes
#set.seed(10000)
#x <- runif(seq(1:n))
## Noise error standard deviation
#sigma <- 0.1
#err <- sigma*rnorm(seq(1:n))
#y <- sin(5*x)+err


#####################################
# REGRESSION WITH POLYNOMIAL MODELS
####################################
# Library for PRESS related Stat
library(qpcR)
Rsq <- vector()
# Stat is a measure of cross-validation (leave-one_out)
Stat <- vector()
# Measure than combines error and model complexity (the smaller the better)
# The basic formula is defined as: AIC = -2(log-likelihood) + 2K
aic_measure <- vector()


mod_lin <- lm(y~x)
Rsq[1] <- summary(mod_lin)$r.squared
Stat[1] <- PRESS(mod_lin)$stat
aic_measure[1] <- AIC(mod_lin)

mod_poli2 <- lm(y~poly(x,2,raw=TRUE))
Rsq[2] <- summary(mod_poli2)$r.squared
Stat[2] <- PRESS(mod_poli2)$stat
aic_measure[2] <- AIC(mod_poli2)

mod_poli3 <- lm(y~poly(x,3,raw=TRUE))
Rsq[3] <- summary(mod_poli3)$r.squared
Stat[3] <- PRESS(mod_poli3)$stat
aic_measure[3] <- AIC(mod_poli3)

mod_poli4 <- lm(y~poly(x,4,raw=TRUE))
Rsq[4] <- summary(mod_poli4)$r.squared
Stat[4] <- PRESS(mod_poli4)$stat
aic_measure[4] <- AIC(mod_poli4)

mod_poli5 <- lm(y~poly(x,5,raw=TRUE))
Rsq[5] <- summary(mod_poli5)$r.squared
Stat[5] <- PRESS(mod_poli5)$stat
aic_measure[5] <- AIC(mod_poli5)

mod_poli6 <- lm(y~poly(x,6,raw=TRUE))
Rsq[6] <- summary(mod_poli6)$r.squared
Stat[6] <- PRESS(mod_poli6)$stat
aic_measure[6] <- AIC(mod_poli6)

mod_poli7 <- lm(y~poly(x,7,raw=TRUE))
Rsq[7] <- summary(mod_poli7)$r.squared
Stat[7] <- PRESS(mod_poli7)$stat
aic_measure[7] <- AIC(mod_poli7)

mod_poli8 <- lm(y~poly(x,8,raw=TRUE))
Rsq[8] <- summary(mod_poli8)$r.squared
Stat[8] <- PRESS(mod_poli8)$stat
aic_measure[8] <- AIC(mod_poli8)

mod_poli9 <- lm(y~poly(x,9,raw=TRUE))
Rsq[9] <- summary(mod_poli9)$r.squared
Stat[9] <- PRESS(mod_poli9)$stat
aic_measure[9] <- AIC(mod_poli9)

mod_poli10 <- lm(y~poly(x,10,raw=TRUE))
Rsq[10] <- summary(mod_poli10)$r.squared
Stat[10] <- PRESS(mod_poli10)$stat
aic_measure[10] <- AIC(mod_poli10)

# Visualize the evolution of Rsq and Stat with polynomial degree
plot(Rsq)
plot(Stat)
# We can compare only up to given degree
plot(Stat[1:5])

# Visualize evolution of aic_measure
plot(aic_measure[1:4])



##############################################################
# PLOT GRAPHS OF POLYNOMIAL MODELS (CHOOSE THE ONES YOU WANT)
##############################################################
# DATA (Define range in graph)
plot(x,y,ylim=c(-0.5,2))

# Plotting polynomial models
xx <- seq(0,1, length.out=200)
lines(xx, predict(mod_lin, data.frame(x=xx)),col='blue')
Sys.sleep(1)
lines(xx, predict(mod_poli2, data.frame(x=xx)), col='darkcyan')
Sys.sleep(1)
lines(xx, predict(mod_poli3, data.frame(x=xx)), col='yellow')
Sys.sleep(1)
lines(xx, predict(mod_poli4, data.frame(x=xx)), col='violet')
Sys.sleep(1)
lines(xx, predict(mod_poli5, data.frame(x=xx)), col='purple')
Sys.sleep(1)
lines(xx, predict(mod_poli6, data.frame(x=xx)), col='grey')
Sys.sleep(1)
lines(xx, predict(mod_poli7, data.frame(x=xx)), col='brown')
Sys.sleep(1)
#lines(xx, predict(mod_poli8, data.frame(x=xx)), col='violet')
#Sys.sleep(1)
#lines(xx, predict(mod_poli9, data.frame(x=xx)), col='violet')
#Sys.sleep(1)
#lines(xx, predict(mod_poli10, data.frame(x=xx)), col='violet')
#Sys.sleep(1)



##########################################
# K-NEAREST NEIGHBORS REGRESSION
##########################################
library(FNN)

datos_entrenamiento <- x
datos_entrenamiento_salida <- y
# Neighboring data definition
datos_donde_evaluar <- seq(from=0,to=1,by=0.01)
# Insert randomness to undo ties
set.seed(1)
# Command requires data to be given in matrix format 
modelo_knn=knn.reg(datos_entrenamiento,as.matrix(datos_donde_evaluar),datos_entrenamiento_salida,k=3)
lines(datos_donde_evaluar,modelo_knn$pred, col="green")


##########################################################
# GAUSSIAN PROCESS BASED REGRESSION
##########################################################
# We define a function to compute covariance matrix
# l = length of scale parameter
calcSigma <- function(X1,X2,l=0.03) {
  Sigma <- matrix(rep(0, length(X1)*length(X2)), nrow=length(X1))
  for (i in 1:nrow(Sigma)) {
    for (j in 1:ncol(Sigma)) {
      # (Arbitrary) definition of covariances based upon distances in x
      Sigma[i,j] <- exp(-0.5*(abs(X1[i]-X2[j])/l)^2)
    }
  }
  return(Sigma)
}
# Select points where the function is to be defined 
x.star <- seq(0,1,len=200)

# Define known data
f <- data.frame(x,y)
# Compute covariance matrix between x and x.star values
x <- f$x
k.xx <- calcSigma(x,x)
k.xsx <- calcSigma(x.star,x)

# Known theoretical expression for conditioned mean and variance in Multivariate Gaussian RVs
# 'solve' computes inverse of  matrix (sometimes det(k.xx) may be small) 
f.star.bar <- k.xsx%*%solve(k.xx)%*%f$y
lines(x.star,f.star.bar, col="red")
Sys.sleep(1)


#########################
# NEURAL NETS
#########################
# Library for NNs
library("neuralnet")

#Column bind the data into one variable
trainingdata <- data.frame(x,y)

# Define the neural network: it is going to have hidneur neurons in hidden layers
hidneur <- 5
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
net <- neuralnet(y~x,trainingdata, hidden=hidneur, threshold=0.001, rep=10)

#Predictions and plotting
informacion_red <- compute(net, x.star) #Get predictions through the neural network
net.output <- informacion_red$net.result
lines(x.star,net.output, col="black")
