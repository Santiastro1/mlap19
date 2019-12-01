# IMAGE VECTOR QUANTIZATION USING SOM AND KMEANS
################################################################33
# Load libraries
library(ripa)
library(bmp)
library(jpeg)
library(pixmap)
library(kohonen)
library(MASS)
library(cluster)

# Clean environment
rm(list=ls())

# Set working directory: the directory where this source file is located
#library(rstudioapi)
#setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# Read jpeg images: values between 0 and 1
foto1_color <- readJPEG("images/aj.jpg")

# Compute gray picture
foto1 <- 0.21*foto1_color[,,1]+ 0.71*foto1_color[,,2]+ 0.07*foto1_color[,,3]

# Quantize (normalize to 1) and plot picture
foto1_cuantizada <- floor(foto1*255)/255

# Plot image
plot(main="Image 1 quantized",imagematrix(foto1_cuantizada))

# Save into file
png("images/Foto1_cuantizada.png",width=dim(foto1_color)[2],height=dim(foto1_color)[1])
plot(main="Image 1 quantized",imagematrix(foto1_cuantizada))
dev.off()

# Choose block size (square) defining number of pixels of side. It determines number of blocks
block.size <-16
nblock.x <- floor(dim(foto1)[1]/block.size)
nblock.y <- floor(dim(foto1)[2]/block.size)

# QUANTIZATION ERROR
# Trim picture to compare with other errors 
foto1_cortada <- foto1[1:(nblock.x*block.size),1:(nblock.y*block.size)]
foto1_cuantizada_cortada <- foto1_cuantizada[1:(nblock.x*block.size),1:(nblock.y*block.size)]
Error_cuanti <- sum((foto1_cuantizada_cortada-foto1_cortada)^2)


# Reordering blocks into vectors
# As many rows as blocks: each row stores a block 
datos <- matrix(nrow=nblock.x*nblock.y,ncol=block.size*block.size)
for(i in (1:nblock.x)){
  for(j in (1:nblock.y)){
    trozo <- vector()
      for(j1 in (1:block.size)){
       trozo <- c(trozo,foto1[((i-1)*block.size+1):((i-1)*block.size+block.size),(j-1)*block.size+j1])
    }
 datos[(i-1)*nblock.y+j,] <- trozo
  }  
}

# Substitute each row with average value (mean of each block: pixeling)
datos_pix <- t(apply(datos,1,function(x) rep(floor(mean(x)*255)/255,block.size*block.size)))

# Reconstruct pixeled image
foto1_pix <- matrix(nrow=nblock.x*block.size,ncol=nblock.y*block.size)

for(i in (1:nblock.x)){
  for(j in (1:nblock.y)){
    for(j1 in (1:block.size)){
      foto1_pix[((i-1)*block.size+1):((i-1)*block.size+block.size),(j-1)*block.size+j1] <- datos_pix[(i-1)*nblock.y+j,((j1-1)*block.size+1):(j1*block.size)]      
    }
    
  }  
}

# Plot image
plot(main="Pixeled",imagematrix(foto1_pix))

# Save into file
png("images/Foto1_pixelada.png",width=nblock.y*block.size,height=nblock.x*block.size)
plot(main="Pixeled",imagematrix(foto1_pix))
dev.off()


# PIXELING ERROR (in trimmed image, to compare with other errors)
Error_pixel <- sum((foto1_cuantizada_cortada-foto1_pix)^2)


#############
# SOM
#############

# Number of neurons 
neuronas.x <- 16
neuronas.y <- 16

######################################################################################
# Calling Kononen library
######################################################################################
sal_koh <- som(datos, grid=somgrid(neuronas.x,neuronas.y,"rectangular"),rlen = 100, alpha = c(0.05, 0.01),
               keep.data = TRUE)


# Code new fata
# Project the same data as a first trial 
nuevos_datos <- matrix(nrow=nblock.x*nblock.y,ncol=block.size*block.size)
nuevos_datos <- datos

##################################################################################
# Project with libeary Kohonen (map.kohonen section in manual)
proy_kohonen <- map(sal_koh,nuevos_datos)
# If new version of R run next line
#datos_proy <- sal_koh$codes[[1]][proy_kohonen[[1]],]
# If old version of R change previous line by this one
datos_proy <- sal_koh$codes[proy_kohonen[[1]],]

foto1_cod_som <- matrix(nrow=nblock.x*block.size,ncol=nblock.y*block.size)

for(i in (1:nblock.x)){
  for(j in (1:nblock.y)){
    for(j1 in (1:block.size)){
      foto1_cod_som[((i-1)*block.size+1):((i-1)*block.size+block.size),(j-1)*block.size+j1] <- datos_proy[(i-1)*nblock.y+j,((j1-1)*block.size+1):(j1*block.size)]      
    }
 
  }  
}

# Plot image quantized with SOM
plot(main="Image 1 coded with SOM",imagematrix(foto1_cod_som))

# Save into file
png("images/Foto1_cod_SOM.png",width=nblock.y*block.size,height=nblock.x*block.size)
plot(main="Image 1 coded with SOM",imagematrix(foto1_cod_som))
dev.off()

# ERROR OBTAINED WITH SOM
Error_som <- sum((foto1_cuantizada_cortada-foto1_cod_som)^2)


#########################
# K-MEANS
#########################
# K-Means Cluster Analysis: number predefined by desired codebook size
km.out <- kmeans(datos, iter.max= 10, neuronas.x*neuronas.y) #  cluster solution

##################################################################################
# K-means already provides the projection (cluster they belong to) the training data
datos_proy.km <- km.out$centers[km.out$cluster,]

foto1_cod_km <- matrix(nrow=nblock.x*block.size,ncol=nblock.y*block.size)

for(i in (1:nblock.x)){
  for(j in (1:nblock.y)){
    for(j1 in (1:block.size)){
      foto1_cod_km[((i-1)*block.size+1):((i-1)*block.size+block.size),(j-1)*block.size+j1] <- datos_proy.km[(i-1)*nblock.y+j,((j1-1)*block.size+1):(j1*block.size)]      
    }
    
  }  
}

# Plot image coded with K-means
plot(main="Coded with cluster kmeans",imagematrix(foto1_cod_km))

# Save into file
png("images/Foto1_cod_kmeans.png",width=nblock.y*block.size,height=nblock.x*block.size)
plot(main="Coded with cluster kmeans",imagematrix(foto1_cod_km))
dev.off()

# ERROR KMEANS
Error_kmeans <- sum((foto1_cuantizada_cortada-foto1_cod_km)^2)

# Plot them together
par(mfrow=c(1,3))
plot(main="Image 1 pixeled",imagematrix(foto1_pix))
plot(main="Image 1 with SOM1",imagematrix(foto1_cod_som))
plot(main="Image 1 with Kmeans1",imagematrix(foto1_cod_km))

# Save them
png("images/Foto1_pixelada_SOM_y_Kmeans.png",width=3*nblock.y*block.size,height=nblock.x*block.size)
par(mfrow=c(1,3))
plot(main="Image 1 pixeled",imagematrix(foto1_pix))
plot(main="Image 1 with SOM1",imagematrix(foto1_cod_som))
plot(main="Image 1 with Kmeans1",imagematrix(foto1_cod_km))
dev.off()

##############################################################
# CODE NEW IMAGE WITH SAME SOM AND KMEANS WITHOUT RETRAINING
##############################################################

foto2_color <- readJPEG("images/gc.jpg")

# Compute gray picture
foto2 <- 0.21*foto2_color[,,1]+ 0.71*foto2_color[,,2]+ 0.07*foto2_color[,,3]

# Quantize and plot
foto2_cuantizada <- floor(foto2*255)/255
par(mfrow=c(1,1))
plot(main="Image 2 quantized",imagematrix(foto2_cuantizada))

# Save image
png("images/Foto2_cuantizada.png",width=dim(foto2_color)[2],height=dim(foto2_color)[1])
plot(main="Image 2 quantized",imagematrix(foto2_cuantizada))
dev.off()

# Partition into blocks
nblock.x2 <- floor(dim(foto2)[1]/block.size)
nblock.y2 <- floor(dim(foto2)[2]/block.size)

# QUANTIZATION ERROR (in trimmed image cortada to compare with other errors)
foto2_cortada <- foto2[1:(nblock.x2*block.size),1:(nblock.y2*block.size)]
foto2_cuantizada_cortada <- foto2_cuantizada[1:(nblock.x2*block.size),1:(nblock.y2*block.size)]
Error_cuanti2 <- sum((foto2_cuantizada_cortada-foto2_cortada)^2)

# Coding
datos2 <- matrix(nrow=nblock.x2*nblock.y2,ncol=block.size*block.size)
for(i in (1:nblock.x2)){
  for(j in (1:nblock.y2)){
    trozo2 <- vector()
    for(j1 in (1:block.size)){
      trozo2 <- c(trozo2,foto2[((i-1)*block.size+1):((i-1)*block.size+block.size),(j-1)*block.size+j1])
    }
    datos2[(i-1)*nblock.y2+j,] <- trozo2
  }  
}

# Pixeling
# Substitute each line by mean value (mean for each block: pixeling)
datos2_pix <- t(apply(datos2,1,function(x) rep(floor(mean(x)*255)/255,block.size*block.size)))

# Reconstrucy pixeled image
foto2_pix <- matrix(nrow=nblock.x2*block.size,ncol=nblock.y2*block.size)

for(i in (1:nblock.x2)){
  for(j in (1:nblock.y2)){
    for(j1 in (1:block.size)){
      foto2_pix[((i-1)*block.size+1):((i-1)*block.size+block.size),(j-1)*block.size+j1] <- datos2_pix[(i-1)*nblock.y2+j,((j1-1)*block.size+1):(j1*block.size)]      
    }
    
  }  
}

#Plot image
plot(main="Pixeled 2",imagematrix(foto2_pix))

# Save into file
png("images/Foto2_pixelada.png",width=nblock.y2*block.size,height=nblock.x2*block.size)
plot(main="Pixeled 2",imagematrix(foto2_pix))
dev.off()

#  Error (in trimmed image for comparison purposes)
Error_pixel2 <- sum((foto2_cuantizada_cortada-foto2_pix)^2)

# PROJECT NEW DATA
nuevos_datos2 <- datos2

# Coding image 2 with SOM trained with image 1
proy_kohonen2 <- map(sal_koh,nuevos_datos2)
# If new version of R run next line
#datos_proy2 <- sal_koh$codes[[1]][proy_kohonen2[[1]],]
# If you have an old R version change previous line by the following one
datos_proy2 <- sal_koh$codes[proy_kohonen2[[1]],]


foto2_cod_som <- matrix(nrow=nblock.x2*block.size,ncol=nblock.y2*block.size)

for(i in (1:nblock.x2)){
  for(j in (1:nblock.y2)){
    for(j1 in (1:block.size)){
      foto2_cod_som[((i-1)*block.size+1):((i-1)*block.size+block.size),(j-1)*block.size+j1] <- datos_proy2[(i-1)*nblock.y2+j,((j1-1)*block.size+1):(j1*block.size)]
    }
    
  }  
}

# ERROR WITH SOM
Error_som2 <- sum((foto2_cuantizada_cortada-foto2_cod_som)^2)

# Save image
plot(main="Image 2 coded with SOM1",imagematrix(foto2_cod_som))

png("images/Foto2_cod_SOM1.png",width=nblock.y2*block.size,height=nblock.x2*block.size)
plot(main="Image 2 coded with SOM1",imagematrix(foto2_cod_som))
dev.off()

# Code image 2 with KMEANS of image 1
closest.cluster <- function(x) {
  cluster.dist <- apply(km.out$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}
cluster2 <- apply(nuevos_datos2, 1, closest.cluster)
datos_proy2.km <- km.out$centers[cluster2,]

foto2_cod_km <- matrix(nrow=nblock.x2*block.size,ncol=nblock.y2*block.size)

for(i in (1:nblock.x2)){
  for(j in (1:nblock.y2)){
    for(j1 in (1:block.size)){
      foto2_cod_km[((i-1)*block.size+1):((i-1)*block.size+block.size),(j-1)*block.size+j1] <- datos_proy2.km[(i-1)*nblock.y2+j,((j1-1)*block.size+1):(j1*block.size)]
    }
    
  }  
}


# ERROR WITH KMEANS
Error_kmeans2 <- sum((foto2_cuantizada_cortada-foto2_cod_km)^2)

# Plot image
plot(main="Image 2 coded with Kmeans1",imagematrix(foto2_cod_km))

# Save into file
png("images/Foto2_cod_Kmeans1.png",width=nblock.y2*block.size,height=nblock.x2*block.size)
plot(main="Image 2 coded with Kmeans1",imagematrix(foto2_cod_km))
dev.off()


# Plot them together
par(mfrow=c(1,3))
plot(main="Image 2 pixeled",imagematrix(foto2_pix))
plot(main="Image 2 with SOM1",imagematrix(foto2_cod_som))
plot(main="Image 2 with Kmeans1",imagematrix(foto2_cod_km))

# Save them into file 
png("images/Foto2_pixelada_SOM_y_Kmeans.png",width=3*nblock.y2*block.size,height=nblock.x2*block.size)
par(mfrow=c(1,3))
plot(main="Image 2 pixeled",imagematrix(foto2_pix))
plot(main="Image 2 with SOM1",imagematrix(foto2_cod_som))
plot(main="Image 2 with Kmeans1",imagematrix(foto2_cod_km))
dev.off()


##############################################################################
# PLOT THE CODEBOOKS
##############################################################################

# CODEBOOK KMEANS
# Construct image (number of blocks is number of centroids)
datos_codebook.km <- km.out$centers[(1:(neuronas.x*neuronas.y)),]
foto_codebook.km <- matrix(nrow=neuronas.x*block.size,ncol=neuronas.y*block.size)

for(i in (1:neuronas.x)){
  for(j in (1:neuronas.y)){
    for(j1 in (1:block.size)){
      foto_codebook.km[((i-1)*block.size+1):((i-1)*block.size+block.size),(j-1)*block.size+j1] <- datos_codebook.km[(i-1)*neuronas.y+j,((j1-1)*block.size+1):(j1*block.size)]      
    }
    
  }  
}

# Plot it
plot(main="Codebook of kmeans", imagematrix(foto_codebook.km))

jpeg("images/Codebook_kmeans.jpg")
plot(main="Codebook of kmeans", imagematrix(foto_codebook.km))
dev.off()


# CODEBOOK KMEANS
# Construct image (number of blocks is number of neurons)
# If new version of R run next line
#datos_codebook <- sal_koh$codes[[1]][(1:(neuronas.x*neuronas.y)),]
# If you have an old version of R change previous line by next one
datos_codebook <- sal_koh$codes[(1:(neuronas.x*neuronas.y)),]

foto_codebook <- matrix(nrow=neuronas.x*block.size,ncol=neuronas.y*block.size)

for(i in (1:neuronas.x)){
  for(j in (1:neuronas.y)){
    for(j1 in (1:block.size)){
      foto_codebook[((i-1)*block.size+1):((i-1)*block.size+block.size),(j-1)*block.size+j1] <- datos_codebook[(i-1)*neuronas.y+j,((j1-1)*block.size+1):(j1*block.size)]      
    }
    
  }  
}

# Plot it
par(mfrow=c(1,1))
plot(main="Codebook SOM", imagematrix(foto_codebook))

# Save it into file
jpeg("images/Codebook_SOM.jpg")
plot(main="Codebook SOM", imagematrix(foto_codebook))
dev.off()



# Print Errors
sink("Errors")
cat("Error quantization 1 \n")
cat(Error_cuanti)
cat("\n")
cat("Error pixeling image 1 \n")
cat(Error_pixel)
cat("\n")
cat("Error coding SOM image 1 \n")
cat(Error_som)
cat("\n")
cat("Error coding Kmeans image 1 \n")
cat(Error_kmeans)
cat("\n")
cat("Error quatization image 2 \n")
cat(Error_cuanti2)
cat("\n")
cat("Error pixeling image 2 \n")
cat(Error_pixel2)
cat("\n")
cat("Error coding SOM image 2 \n")
cat(Error_som2)
cat("\n")
cat("Error coding Kmeans image 2 \n")
cat(Error_kmeans2)
cat("\n")
sink()

