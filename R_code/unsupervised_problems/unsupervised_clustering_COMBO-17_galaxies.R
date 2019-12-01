###############################################################################
# ADAPTED FROM "Modern Statistical Methods for Astronomy", by Feigelson & Babu
##############################################################################

# Clean all
rm(list=ls())
# Color-magnitude diagram for low-redshift COMBO-17 galaxies
#COMBO_loz=read.table("http://astrostatistics.psu.edu/MSMA/datasets/COMBO17_lowz.dat", header=T, fill=T)

# Set path
#library(rstudioapi)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
COMBO_loz=read.table("../data/COMBO17_lowz.dat", header=T, fill=T)
dim(COMBO_loz) ; names(COMBO_loz)
par(mfrow=c(1,2))
plot(COMBO_loz, pch=20, cex=0.5, xlim=c(-22,-7), ylim=c(-2,2.5), xlab=expression(M[B]~~(mag)), ylab=expression(M[280] - M[B]~~(mag)), main="")


# Two-dimensional kernel-density estimator
library(MASS)
COMBO_loz_sm <- kde2d(COMBO_loz[,1], COMBO_loz[,2], h=c(1.6,0.4),
                      lims = c(-22,-7,-2,2.5), n=500)
image(COMBO_loz_sm, col=grey(13:0/15), xlab=expression(M[B]~~(mag)),
      ylab=expression(M[280] - M[B]~~(mag)), xlim=c(-22,-7), ylim=c(-2,2.5),
      xaxp=c(-20,-10,2))
par(mfrow=c(1,1))



#############################################
# Standardize variables
Mag_std <- scale(COMBO_loz[,1])
Color_std <- scale(COMBO_loz[,2])
COMBO_std <- cbind(Mag_std,Color_std)
# Hierarchical clustering
COMBO_dist <- dist(COMBO_std)
COMBO_hc <- hclust(COMBO_dist, method="complete")
COMBO_coph <- cophenetic(COMBO_hc)
cor(COMBO_dist, COMBO_coph)
# Cutting the tree at k=5 clusters
plclust(COMBO_hc, label=F)
COMBO_hc5a <- rect.hclust(COMBO_hc, k=5, border='black')
str(COMBO_hc5a)
COMBO_hc5b <- cutree(COMBO_hc, k=5)
str(COMBO_hc5b)
plot(COMBO_loz, pch=(COMBO_hc5b+19), cex=0.7, xlab=expression(M[B]~~(mag)),
     ylab=expression(M[280] - M[B]~~(mag)), main="")


######################################################################
# Density-based clustering algorithm
#install.packages("fpc") ; 
library(fpc)
COMBO_dbs <- dbscan(COMBO_std, eps=0.1, MinPts=5, method="raw")
print.dbscan(COMBO_dbs) ; COMBO_dbs$cluster
plot(COMBO_loz[COMBO_dbs$cluster==0,], pch=20, cex=0.7, xlab="M_B (mag)",
     ylab="M_280 - M_B (mag)")
points(COMBO_loz[COMBO_dbs$cluster==2,], pch=2, cex=1.0)
points(COMBO_loz[COMBO_dbs$cluster==1 | COMBO_dbs$cluster==3,], pch=1, cex=1.0)

##############################################################
# Model-based clustering
library(mclust)
COMBO_mclus <- mclustBIC(COMBO_loz,modelNames="VVV")
plot(COMBO_mclus, col="black")
COMBO_sum_mclus <- summary.mclustBIC(COMBO_mclus,COMBO_loz,3)
COMBO_sum_mclus$parameters ; COMBO_sum_mclus$classification
COMBO_sum_mclus$z ; COMBO_sum_mclus$uncertainty
plot(COMBO_loz, pch=(19+COMBO_sum_mclus$classification), cex=1.0, xlab="M_B (mag)",
     ylab="M_280 - M_B (mag)", main="COMBO-17 MVN model clustering (k=3)",
     cex.lab=1.3, cex.axis=1.3)

