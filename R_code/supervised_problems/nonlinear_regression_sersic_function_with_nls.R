###############################################################################
# ADAPTED FROM "Modern Statistical Methods for Astronomy", by Feigelson & Babu
# Fit Sersic function to NGC 4472 elliptical galaxy surface brightness profile
###############################################################################
# Clean environment
rm(list=ls())

# Reading data (from http)
#library(httr)
#set_config(config(ssl_verifypeer = 0L))
#NGC4472 <- read.table("http://astrostatistics.psu.edu/MSMA/datasets/NGC4472_profile.dat",header=T)

# Read data via path  from source directory
#source_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
#setwd(source_directory)
NGC4472 <- read.table("../data/NGC4472_profile.dat",header=T)
attach(NGC4472)

# Nonlinear Least Squares
NGC4472.fit <- nls(surf_mag ~ -2.5*log10(I.e * 10^(-(0.868*n-0.142)*
                                                     ((radius/r.e)^{1/n}-1))) + 26, data=NGC4472, start=list(I.e=20.,
                                                                                                             r.e=120.,n=4.), model=T, trace=T)
summary(NGC4472.fit)

# Quality measures
deviance(NGC4472.fit)
logLik(NGC4472.fit)
AIC(NGC4472.fit)

# Plot NGC 4472 data and best-fit model
plot(NGC4472.fit$model$radius, NGC4472.fit$model$surf_mag, pch=20,
     xlab="r (arcsec)", ylab=expression(mu ~~ (mag/sq.arcsec)), ylim=c(16,28),
     cex.lab=1.5, cex.axis=1.5)
lines(NGC4472.fit$model$radius, fitted(NGC4472.fit))


# DATA FROM OTHER GALAXY NGC 4406 
# Fit and plot radial profiles 
#NGC4406 <-
#  read.table("http://astrostatistics.psu.edu/MSMA/datasets/NGC4406_profile.dat",
#             header=T)
NGC4406 <- read.table("../data/NGC4406_profile.dat",header=T)
attach(NGC4406)
NGC4406.fit <- nls(surf_mag ~ -2.5*log10(I.e * 10^(-(0.868*n-0.142)*
                                                     ((radius/r.e)^{1/n}-1))) + 32, data=NGC4406, start=list(I.e=20.,
                                                                                                             r.e=120.,n=4.), model=T, trace=T)
summary(NGC4406.fit)
points(NGC4406.fit$model$radius, NGC4406.fit$model$surf_mag, pch=3)
lines(NGC4406.fit$model$radius, fitted(NGC4406.fit))


# DATA FROM OTHER GALAXY NGC 4551
#NGC4551 <-
#  read.table("http://astrostatistics.psu.edu/MSMA/datasets/NGC4551_profile.dat",
#             header=T)
NGC4551 <- read.table("../data/NGC4551_profile.dat",header=T)
attach(NGC4551)
NGC4551.fit <- nls(surf_mag ~ -2.5*log10(I.e * 10^(-(0.868*n-0.142)*
                                                     ((radius/r.e)^{1/n}-1))) + 26, data=NGC4551, start=list(I.e=20.,r.e=15.,n=4.),
                   model=T, trace=T)
summary(NGC4551.fit)
points(NGC4551.fit$model$radius, NGC4551.fit$model$surf_mag, pch=5)
lines(NGC4551.fit$model$radius, fitted(NGC4551.fit))
legend(500, 20, c("NGC 4472","NGC 4406", "NGC 4551"), pch=c(20,3,5))



##############################################################################
# NGC 4472 ANALYSIS
##############################################################################
# Residual plot
plot(NGC4472.fit$model$radius,residuals(NGC4472.fit), xlab="r (arcsec)",
     ylab="Residuals", pch=20, cex.lab=1.5, cex.axis=1.5)
lines(supsmu(NGC4472.fit$model$radius, residuals(NGC4472.fit), span=0.05),
      lwd=2)
# Test for normality of residuals
qqnorm(residuals(NGC4472.fit) / summary(NGC4472.fit)$sigma)
abline(a=0,b=1)
shapiro.test(residuals(NGC4472.fit) / summary(NGC4472.fit)$sigma)
# Bootstrap parameter estimates
#install.packages('nlstools') 
library(nlstools)

NGC4472.boot <- nlsBoot(NGC4472.fit)
summary(NGC4472.boot)
curve(dnorm(x,m=5.95, sd=0.10)*58/5.95, xlim=c(5.6,6.4), ylim=c(0,50))
hist(NGC4472.boot$coefboot[,3], breaks=50, add=T) # not shown
