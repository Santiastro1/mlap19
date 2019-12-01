###############################################################################
# ADAPTED FROM "Modern Statistical Methods for Astronomy", by Feigelson & Babu
##############################################################################

# Clean all
rm(list=ls())

#source_directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
#setwd(source_directory)
# Overview of globular cluster properties
#GC = read.table("http://astrostatistics.psu.edu/MSMA/datasets/GlobClus_prop.dat",
#                header=T, fill=T)
GC <- read.table("../data/GlobClus_prop.dat",header=T, fill=T)
summary(GC)
manyhist <- function(x) {
  par(mfrow=n2mfrow(dim(x)[2]))
  for (i in 1:dim(x)[2]) { name=names(x)[i]
  hist(x[,i], main="", breaks='FD', ylab="", xlab=name) }}
par(mfrow=c(1,1))
manyhist(GC[,2:13])



# Prepare the data
# 1. Remove objects with NA entries, remove labels
dim(GC) ; GC1 <- na.omit(GC[,-1]) ; dim(GC1)
# 2. Standardize variables
GC2 <- scale(GC1)
# 3. Separate locational and dynamical variables
GCloc <- GC2[,+c(1:4)]
GCdyn <- GC2[,-c(1:4)]
# 4. Remove two bad outliers
GCdyn1 <- GCdyn[-c(which.max(GCdyn[,4]), which.max(GCdyn[,11])),]
GCloc1 <- GCloc[-c(which.max(GCdyn[,4]), which.max(GCdyn[,11])),]

# Bivariate relationships
cor(GCdyn1, method='kendall')
var(GCdyn1)
pairs(GCdyn1[,3:8],pch=20,cex=0.4)


# Elaborated color pairs plot
library(MASS)
pairs(GCdyn1[,5:8],main='',labels=names(GCdyn1[5:8]), panel=function(x,y) {
  + abline(lsfit(x,y)$coef,lwd=2,col='deeppink2')
  + lines(lowess(x,y),lwd=3,col='blue3',lty=2)
  + points(x,y,pch=21,bg = c("red", "green3", "blue"))
  + rug(jitter(x,factor=3),side=1,col='lightcoral',ticksize=-.05)
  + rug(jitter(y,factor=3),side=2,col='cornflowerblue',ticksize=-.05)
  + contour(kde2d(x,y)$x, kde2d(x,y)$y, kde2d(x,y)$z, drawlabels=F,add=T,
            col='darkblue',nlevel=4)})

# PCA for dynamical variables.
PCdyn <- princomp(GCdyn1)
plot(PCdyn, main='')
summary(PCdyn)
loadings(PCdyn)
biplot(PCdyn, col='black', cex=c(0.6,1))
# Add principal component values into the dataframe
PCdat <- data.frame(names=row.names(GCdyn1), GCdyn1, PCdyn$scores[,1:4])

