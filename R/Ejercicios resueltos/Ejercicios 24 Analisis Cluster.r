################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica 
##
## Autor: Jes�s Herranz
## 
## Ejercicios Sesi�n 24: An�lisis Cluster
##                   
## Fichero de datos: ALLSubset
##
################################################################################
################################################################################

################################################################################
## Fichero Datos: ALLSubset
################################################################################

xx <- read.delim( "C://Bioestadistica con R/Datos/ALLSubset.txt", sep=" ")
dim(xx)


################################################################################
## An�lisis Cluster Jer�rquico
################################################################################

## Average linkage (distancia media)
clust.med <- hclust ( dist ( xx [ , 1:1000] , method="euclidean" ), 
                      method = "average")

dev.new()
plot( clust.med, main="Average linkage", lab=xx$mol.biol,
      ylab="Distance", xlab="", sub="", cex=0.8 )   

 
################################################################################
## An�lisis Cluster k-means
################################################################################

##########################################################
## K-means con elecci�n aleatoria de los centros
km.out <- kmeans(  xx [ , 1:1000]   , cent=2 )

km.out$cluster

table(km.out$cluster)
table(km.out$cluster, xx$mol.biol )


################################################################################
## PCA                                               
################################################################################

pca.out <- prcomp( xx [ , 1:1000] , scale=T )
summary(pca.out)

## Scores en las CPs 1 y 2
dev.new()
plot( pca.out$x[ , 1], pca.out$x[ , 2], xlab="CP1", ylab="CP2", 
      col=xx$mol.biol, pch=16 )
 
## Scores en las CPs 5 y 7
dev.new()
plot( pca.out$x[ , 5], pca.out$x[ , 7], xlab="CP5", ylab="CP7", 
      col=xx$mol.biol, pch=16 )
       
       