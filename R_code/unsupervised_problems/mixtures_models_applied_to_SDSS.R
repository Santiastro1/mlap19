###############################################################################
# ADAPTED FROM "Modern Statistical Methods for Astronomy", by Feigelson & Babu
##############################################################################

# Clean all
rm(list=ls())

# r-band distribution of Sloan quasars
#SDSS_qso <- read.table("http://astrostatistics.psu.edu/MSMA/datasets/SDSS_17K.dat", header=T)

#library(rstudioapi)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
SDSS_qso <- read.table("../data/SDSS_17K.dat",header=T)
dim(SDSS_qso) ; summary(SDSS_qso)
qso_r <- SDSS_qso[,5] ; n_qso <- length(qso_r)
par(mfrow=c(1,2))
hist(qso_r, breaks=100, main="", xlim=c(16,26))

# Normal mixture model
#install.packages("mclust"") ; 
library(mclust)
fit.mm <- Mclust(qso_r,modelNames="V")
plot(fit.mm, ylim=c(-67000,-65000))
str(fit.mm)
fit.mm$parameters$mean
sqrt(fit.mm$parameters$variance$sigmasq)
fit.mm$parameters$pro


################################################
par(mfrow=c(1,1))
plot(ecdf(qso_r), xlim=c(16,26), xlab="r (mag)", main="")
r.seq <- seq(10,30,0.02)
sum.comp <- rep(0,length(r.seq))
for (i in 1:fit.mm$G) { sum.comp <- sum.comp + pnorm(r.seq, mean=
                                                       fit.mm$parameters$mean[[i]], sd=sqrt(fit.mm$parameters$variance$sigmasq[[i]])) *
  fit.mm$parameters$pro[[i]] }
lines(r.seq, sum.comp, lwd=3, lty=2)
#for (i in 1:fit.mm$G) {lines(r.seq, dnorm(r.seq, mean=fit2$parameters$mean[[i]],
#                                          sd=sqrt(fit2$parameters$variance$sigmasq[[i]])) * fit2$parameters$pro[[i]]) }

for (i in 1:fit.mm$G) {lines(r.seq, dnorm(r.seq, mean=fit.mm$parameters$mean[[i]],
                                          sd=sqrt(fit.mm$parameters$variance$sigmasq[[i]])) * fit.mm$parameters$pro[[i]]) }


########################################################################
# R script for constructing SDSS test and training datasets is given
# in Appendix C.


# Unsupervised k-means partitioning
SDSS.kmean <- kmeans(SDSS_test,6)
print(SDSS.kmean$centers)
plot(SDSS_test[,1], SDSS_test[,2], pch=20, cex=0.3, col=gray(SDSS.kmean$cluster/7),
     xlab="u-g (mag)", ylab="g-r (mag)", xlim=c(-0.5,3), ylim=c(-0.6,1.5))
