##########################################################################
# Different clustering methods for a dataframe with driver features
##########################################################################
# Clean the environment
rm(list=ls())
set.seed(33)

# Read data
df <- readRDS("../data/drivers.RDS", refhook = NULL)
colnames(df)

# New data frame (df.clean) where to clean data (preserve original df)
df.clean <- df

################################################
#Remove negative values
################################################
df.clean[df.clean <0] <- 0

################################################
# Filter outliers
################################################
# Proportion to multiply IQR
proportion_IQR <- 1.5

for (i in 3:8)
  {
  df.clean <- df.clean[df.clean[,i] > quantile(df.clean[,i], .25) - proportion_IQR*IQR(df.clean[,i]) & 
            df.clean[,i] < quantile(df.clean[,i], .75) + proportion_IQR*IQR(df.clean[,i]), ]
}

################################################
# Normalize columnwise
################################################
df.clean[,3:8] <- apply(df.clean[, 3:8], 2, function(x) (x - min(x))/(max(x)-min(x)))


################################################
# Correlations
################################################
cor(df.clean[,3:8])


################################################
# K-means
################################################
library(cluster)

wss <- (nrow(df.clean[,3:8])-1)*sum(apply(df.clean[,3:8],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(df.clean[,3:8],centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-Means Cluster Analysis: number of clusters chose where "elbow" in graph
n.clusters <- 10
# Solucion con n.clusters clusters
km.out <- kmeans(dist(df.clean[,3:8]), n.clusters) 
# get cluster means
aggregate(df.clean[,3:8],by=list(km.out$cluster),FUN=mean)
# append cluster assignment
df.clean_con_cluster <- data.frame(df.clean, km.out$cluster) 

# vary parameters for most readable graph
clusplot(df.clean[,3:8], km.out$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)


################################################
# HIERARCHICAL CLUSTERING
################################################

# We use standard distance
agrupamiento <- hclust(dist(df.clean[,3:8]))
plot(agrupamiento)

# Represent color maps
library(gplots)
heatmap.2(data.matrix(df.clean[,3:8]),main="Hierarchical Cluster", Rowv=as.dendrogram(agrupamiento), 
          Colv=NA,dendrogram="row", scale="row",density.info="none",trace="none")




########################################################
# SOM (evaluate with both som and kohonen libraries)
########################################################
# In this example SOM does provide dimensionality reduction
library(som)
# Dimension del mapa
dimX <- 3
dimY <- 3
data_train_matrix <- as.matrix(scale(df.clean[,3:8]))
som.out <- som(data_train_matrix,xdim=dimX,ydim=dimY,alphaType="inverse",neig="gaussian",topol="hexa",
               init="sample",radius=c(3,1),rlen=c(100,1000))
plot(som.out)
# Code vector xdim*ydim rowwise rownumber=x+y*xdim
print(som.out$code)

detach("package:som", unload=TRUE)

######################################################################################
# If kohonen library is loaded (different from som library), a new functionality is given 
# to 'som' and the previous 'som' command associated with library som does not work anymore
library(kohonen)
som_grid <- somgrid(xdim = 3, ydim=3, topo="hexagonal")
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=500, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE )

# Plot code vectors
plot(som_model, type="codes", main="Code vectors")
# Plot the number of objects mapped to the individual units
plot(som_model, type="count", main="Node Counts")
# Plot sum of the distances to all immediate neighbours
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances")


detach("package:kohonen", unload=TRUE)

