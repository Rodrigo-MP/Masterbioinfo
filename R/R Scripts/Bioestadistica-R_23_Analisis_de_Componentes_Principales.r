################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##
## Sesión 23: Análisis de Componentes Principales (PCA)
##
################################################################################
################################################################################
                      
library(corrplot)                                         


################################################################################
## Fichero Datos: Prostate 
################################################################################

xx <- read.delim("C://Bioestadistica con R/Datos/Prostate.txt", sep="\t", header=T)
dim(xx)
head(xx)

## Variable binaria con la mediana de lpsa (resp.) como pto de corte (para gráficos)  
lpsa.gr = as.numeric ( xx$lpsa > median(xx$lpsa) )
table(lpsa.gr)

################################################################################
## 1.- Análisis de Componentes Principales
################################################################################

#################################################
## Correlaciones de las variables predictoras continuas

cor.matr = cor(xx[c(2:5,7:9)], method = c("pearson") )
round( cor.matr, 4 )

dev.new()
pairs(xx[c(2:5,7:9)], cex=0.8)

#################################################
## Gráfico de la matriz de Correlaciones
dev.new()
corrplot ( cor.matr , order = "hclust" )


#################################################
## Análisis de Componentes Principales (PCA) sobre la matriz R
pca.out <- prcomp( xx[c(2:5,7:9)], scale=T )
summary(pca.out)

## Varianza explicada por cada CP. Valores propios (lambdas, eigenvalues)
eigen <- pca.out$sdev ** 2
eigen 
sum(eigen)
100 * ( eigen / sum(eigen) )    ## Comprobamos la varianza explicada por las CPs

## Cargas Factoriales
round ( pca.out$rotation , 3 )

## Biplot
dev.new()
biplot(pca.out)

## Gráfico de dispersión en las 2 primeras componentes
dim(pca.out$x)
cp1 <- pca.out$x[ ,1]
cp2 <- pca.out$x[ ,2]
cor(cp1, cp2)
## cp1 = predict ( pca.out , newdata = xx ) [ , 1] el mismo resultado

dev.new()
plot( cp1, cp2, xlab="CP1", ylab="CP2", col=lpsa.gr, pch=16 )
legend ( 4.5, 3 , leg=names(table(lpsa.gr)), palette()[1:2], cex=0.8)

## Gráfico alternativo con etiquetas de las Clases en lugar de puntos
dev.new()
plot( cp1, cp2, xlab="CP1", ylab="CP2", type ="n" )
text ( cp1, cp2, labels=lpsa.gr )

#################################################
## Número de Componentes Principales (Scree Plot)
summary(pca.out)
eigen
dev.new()
plot( pca.out, type ="l", main="Scree Plot")
 

################################################################################
## 2.- Análisis de Componentes Principales. Alta Dimensionalidad
################################################################################

#################################################
## Fichero Datos: ALLSubset
xx2 <- read.delim("C://Bioestadistica con R/Datos/ALLSubset.txt", sep=" ")
dim(xx2)

#################################################
## Análisis de Componentes Principales (PCA)
pca.out.2 <- prcomp( xx2[1:1000], scale=T )
summary(pca.out.2)

## Loadings
dim(pca.out.2$rotation)
pca.out.2$rotation[1:5, 1:5]

## Scores
dim(pca.out.2$x)
pca.out.2$x[1:5, 1:5]

###################
## Gráfico de dispersión en las 2 primeras componentes
dev.new()
biplot( pca.out.2 )
dev.new()
plot ( pca.out.2$x[ , 1], pca.out.2$x[ , 2] , xlab="CP1", ylab="CP2", 
       col=xx2$mol.biol, pch=16 )
legend ( 22, -28, names(table(xx2$mol.biol)), palette(), cex=0.8 )


################################################################################
## 3.- Análisis de Componentes Principales. Función princomp()
################################################################################

#################################################
## Análisis de Componentes Principales (PCA) sobre la matriz R
pca.out.3 <- princomp( xx[c(2:5,7:9)], cor=T )
summary(pca.out.3)

## Varianza explicada por cada CP. Valores propios (lambdas, eigenvalues)
eigen <- pca.out.3$sdev **2
eigen 
sum(eigen)
100 * ( eigen / sum(eigen) )    ## Comprobamos la varianza explicada por las CPs

## Cargas Factoriales
summary(pca.out.3, loadings=T)

## Biplot
dev.new()
biplot(pca.out.3)

## Gráfico de dispersión en las 2 primeras componentes
dim(pca.out.3$scores)
cp1 <- pca.out.3$scores[ ,1]
cp2 <- pca.out.3$scores[ ,2]
cor(cp1, cp2)

dev.new()
plot( cp1, cp2, xlab="CP1", ylab="CP2", col=lpsa.gr, pch=16 )
legend ( -5.5, -2.5 , leg=names(table(lpsa.gr)), palette()[1:2], cex=0.8)


#################################################
## Número de Componentes Principales (Scree Plot)
summary(pca.out.3)
eigen
dev.new()
plot( pca.out.3, type ="l", main="Scree Plot")



