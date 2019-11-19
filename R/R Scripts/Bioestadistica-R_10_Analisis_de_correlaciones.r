################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica
##
## Autor: Jes�s Herranz
##
## Sesi�n 10: An�lisis de correlaciones
##
################################################################################
################################################################################

library(KernSmooth)
library(corrplot)


################################################################################
## Fichero Datos: cystic fibrosis
################################################################################

xx <- read.csv(file="/home/rodrigo/github/Masterbioinfo/R/Datos/cystic fibrosis.csv", sep=";")
dim(xx)
head(xx)

## Variables categ�ricas

xx$sex <- factor(xx$sex)


################################################################################
## Correlaciones
################################################################################

## Correlaci�n: Pearson, Spearman, Kendall
cor ( xx$height, xx$weight , method = "pearson", use="pairwise.complete.obs")
cor ( xx$height, xx$weight , method = "spearman", use="pairwise.complete.obs")
cor ( xx$height, xx$weight , method = "kendall", use="pairwise.complete.obs")

## Matriz de correlaciones
cor ( xx[,4:11], method = "pearson", use="pairwise.complete.obs")

## Test del coeficiente de correlaci�n H0: r = 0
cor.test ( xx$height, xx$weight , method = "pearson", use="pairwise.complete.obs")

## Explorando las asociaciones entre las variables
dev.new()
pairs ( xx[ , c(2,4:11) ] )


################################
## Gr�fico de la matriz de correlaciones

w.cor = cor ( xx[,4:11], method = "pearson", use="pairwise.complete.obs")
dev.new()
corrplot ( w.cor )
dev.new()
corrplot ( w.cor , order="hclust" )


################################
## Gr�fico de dispersi�n con l�neas de contorno

dev.new()
plot( xx$bmp, xx$frc ,pch=16, xlab="BMP", ylab="FRC", col="blue")

## A�adimos las l�neas de contorno
df.graph <- data.frame (  x=xx$bmp, y=xx$frc )
graph.schema <- bkde2D( df.graph , bandwidth = sapply(df.graph, dpik))

contour (x = graph.schema$x1, y = graph.schema$x2, z = graph.schema$fhat,
         add=TRUE, lwd=1.2, nlevels=5)


################################
## Gr�fico de densidades bidimensionales

## Se cargan las funciones
source ( "C://Bioestadistica con R/R Scripts/Bioestad�stica-R Funciones.r")

## Se estima la funci�n de densidad bidimensional
den1 <- bivden(  xx$bmp, xx$frc )

## Gr�fico 3D
dev.new()
persp (den1$seqx, den1$seqy, den1$den, theta=50, phi=40, col="red",
       xlab="BMP", ylab="FRC", zlab="Densidad")
