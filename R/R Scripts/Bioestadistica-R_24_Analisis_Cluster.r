################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##
## Sesión 24: Análisis Cluster
##    
################################################################################
################################################################################

################################################################################
## Fichero Datos: Microarray
################################################################################

xx <- read.delim( "C://Bioestadistica con R/Datos/NCI Microarray.txt", 
                  sep=" ", header=F )
dim(xx)
xx[1:3,1:10]

## Fichero con el Tipo de tumor (una sola columna)
yy <- read.delim( "C://Bioestadistica con R/Datos/NCI Microarray Type Tumors.txt", 
                  sep=" ", header=F )
dim(yy)
head(yy)

## Tipo de tumor (para etiquetar las observaciones)
w.type.tum = yy$V1


################################################################################
## Análisis Cluster Jerárquico
################################################################################

## Single linkage (distancia mínima)
clust.min <- hclust ( dist (  t(xx), method="euclidean" ) , method = "single" )
## Complete linkage (distancia máxima)
clust.max <- hclust ( dist (  t(xx), method="euclidean" ) , method = "complete")
## Average linkage (distancia media)
clust.med <- hclust ( dist (  t(xx), method="euclidean" ) , method = "average")


#################################################
## Gráficos Dendogramas
dev.new()
plot( clust.med, main="Average linkage", lab=w.type.tum,
     ylab="Distance", xlab="", sub="", cex=0.8 )  

dev.new(width=14, height=7)
par(mfrow = c(1,3) )
plot( clust.min, main="Single linkage", lab=w.type.tum, 
      ylab="Distance", xlab="", sub="", cex=0.6 )
plot( clust.max, main="Complete linkage", lab=w.type.tum,
      ylab="Distance", xlab="", sub="", cex=0.6 )
plot( clust.med, main="Average linkage", lab=w.type.tum,
      ylab="Distance", xlab="", sub="", cex=0.6 )


#################################################
## Obtener la solución con 2 clusters
clust.2 <- cutree ( clust.med , k=2 )
clust.2
table(clust.2)


#################################################
## Distancia basada en Correlaciones entre observaciones
## Importante: xx está ya traspuesto (observaciones en las columnas)

w.corr.obs = cor ( xx , method = c("pearson") )
dim(w.corr.obs)
w.dist = as.dist( 1 - w.corr.obs )

## Average linkage (distancia media)
clust.med.corr = hclust ( w.dist , method = "average")
dev.new()
plot( clust.med.corr , main="", lab=w.type.tum, ylab="", xlab="", 
      sub="", cex=0.6, cex.axis=0.8 )  


#################################################
## Heatmap

dev.new()
heatmap ( as.matrix(xx) ,  Rowv=NA , labCol=w.type.tum )


## Ejemplo para un subconjunto cambiando la distancia y el método de link
library(gplots) ## para la escala de color
dist.pear  <- function(x) as.dist ( 1 - cor(t(x)) )
hclust.ave <- function(x) hclust ( x , method="average" )
dev.new()
heatmap ( as.matrix( xx[1:100, ] ) , distfun=dist.pear, hclustfun=hclust.ave,   
          labCol=w.type.tum, col=greenred(10) )



################################################################################
## Análisis Cluster k-means
################################################################################

##########################################################
## K-means con elección aleatoria de los centros

km.out <- kmeans( t(xx)  , cent=2 )

km.out$cluster

table(km.out$cluster)
table(km.out$cluster, w.type.tum)


##########################################################
## K-means eligiendo los centros

centros <- matrix ( NA, 2, nrow(xx) ) ## 2 centros, 6830 variables
length ( xx [ , 1 ] )
centros[1, ] <- xx [ , 10 ]
centros[2, ] <- xx [ , 20 ]
centros [ , 1:10]
                                          
## K-means
km.out.2 <- kmeans( t(xx) , cent = centros )

## Comprobando los resultados con el anterior cluster
table ( km.out$cluster, km.out.2$cluster )


##########################################################
## Número óptimo de clusters. Gráfico probando entre k=2 y k=6 

withinss.sum <- rep(NA, 6)

## SS de todos los datos, antes del clustering: es la suma de las varianzas de
## todas las variables multiplicado por el número de observaciones - 1
n <- ncol(xx)
withinss.sum[1] <- sum (  apply ( xx, 1 , var ) ) * ( n - 1 )   

for ( k in 2:6 )
{ withinss.sum[k] <- kmeans( t(xx) , cent = k )$tot.withinss    }
withinss.sum

# Gráfico de SS
dev.new()
plot( 1:6, withinss.sum, type="l", xlab="Número clusters", 
                          ylab="Suma de cuadrados intra-clusters")


##########################################################
## K-means con K=3 ejecutado 20 veces

km.out <- kmeans( t(xx) , cent=3 , nstart=20 )

km.out$cluster

table(km.out$cluster)
table(km.out$cluster, w.type.tum )


##########################################################
## PCA

pca.out <- prcomp( t (xx ) , scale=T )
summary(pca.out)

## Scores
dim(pca.out$x)
pca.out$x[1:5, 1:5]

Cols = function (vec) 
{ cols = rainbow(length(unique(vec)))  
  cols [ as.numeric ( as.factor (vec)) ] 
}

dev.new()
plot ( pca.out$x[ , 1], pca.out$x[ , 2] , xlab="CP1", ylab="CP2", cex= 1.2,
       col=Cols(w.type.tum) , pch=16 )

