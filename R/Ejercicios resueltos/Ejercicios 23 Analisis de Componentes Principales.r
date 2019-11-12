################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica 
##
## Autor: Jes�s Herranz
## 
## Ejercicios Sesi�n 23: An�lisis de Componentes Principales (PCA)
##                   
## Fichero de datos: Virco
##
################################################################################
################################################################################

library(corrplot)


################################################################################
## Fichero Datos: Virco
################################################################################

xx <- read.csv("C://Bioestadistica con R/Datos/Virco_data.csv", sep=";")
dim(xx)

xx$sens.NFV = factor(xx$sens.NFV)


################################################################################
## 1.- An�lisis de Componentes Principales
################################################################################

#################################################
## Correlaciones

cor.matr = cor ( xx[ 1:89 ], method = c("pearson") )

dev.new()
corrplot ( cor.matr , order = "hclust" )


#################################################
## An�lisis de Componentes Principales (PCA) sobre R
pca.out <- prcomp( xx[ 1:89 ] , scale=T )
summary(pca.out)

## Valores propios (lambdas, eigenvalues)
eigen <- pca.out$sdev **2
eigen     
sum(eigen)

sum( eigen > 1 )  ## N�mero de valores propios mayores que 1

## Biplot
dev.new()
biplot(pca.out)

