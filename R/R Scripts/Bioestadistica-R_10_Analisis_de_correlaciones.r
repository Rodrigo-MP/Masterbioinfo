################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
## 
## Sesión 10: Análisis de correlaciones
##
################################################################################
################################################################################

library(KernSmooth)
library(corrplot)


################################################################################
## Fichero Datos: cystic fibrosis
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/cystic fibrosis.csv", sep=";")
dim(xx)
head(xx)

## Variables categóricas

xx$sex <- factor(xx$sex)


################################################################################
## Correlaciones
################################################################################

## Correlación: Pearson, Spearman, Kendall
cor ( xx$height, xx$weight , method = "pearson", use="pairwise.complete.obs")
cor ( xx$height, xx$weight , method = "spearman", use="pairwise.complete.obs")
cor ( xx$height, xx$weight , method = "kendall", use="pairwise.complete.obs")

## Matriz de correlaciones
cor ( xx[,4:11], method = "pearson", use="pairwise.complete.obs")

## Test del coeficiente de correlación H0: r = 0
cor.test ( xx$height, xx$weight , method = "pearson", use="pairwise.complete.obs")

## Explorando las asociaciones entre las variables
dev.new()
pairs ( xx[ , c(2,4:11) ] )


################################
## Gráfico de la matriz de correlaciones

w.cor = cor ( xx[,4:11], method = "pearson", use="pairwise.complete.obs")
dev.new()
corrplot ( w.cor )
dev.new()
corrplot ( w.cor , order="hclust" ) 


################################
## Gráfico de dispersión con líneas de contorno

dev.new()
plot( xx$bmp, xx$frc ,pch=16, xlab="BMP", ylab="FRC", col="blue")

## Añadimos las líneas de contorno
df.graph <- data.frame (  x=xx$bmp, y=xx$frc )
graph.schema <- bkde2D( df.graph , bandwidth = sapply(df.graph, dpik))

contour (x = graph.schema$x1, y = graph.schema$x2, z = graph.schema$fhat, 
         add=TRUE, lwd=1.2, nlevels=5)

                                                                         
################################
## Gráfico de densidades bidimensionales        

## Se cargan las funciones
source ( "C://Bioestadistica con R/R Scripts/Bioestadística-R Funciones.r") 

## Se estima la función de densidad bidimensional
den1 <- bivden(  xx$bmp, xx$frc )

## Gráfico 3D
dev.new()
persp (den1$seqx, den1$seqy, den1$den, theta=50, phi=40, col="red", 
       xlab="BMP", ylab="FRC", zlab="Densidad") 

