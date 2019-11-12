################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
## 
## Ejercicios Sesión 10: Análisis de Correlaciones
##
## Fichero de datos: Bajo peso al nacer
##    
################################################################################
################################################################################

################################################################################
## Fichero Datos: Bajo peso al nacer
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/Bajo peso al nacer.csv", sep=";")
dim(xx)
head(xx)


################################################################################
## Correlaciones
################################################################################

## Correlación: Pearson, Spearman, Kendall
cor ( xx$edad, xx$peso , method = "pearson", use="pairwise.complete.obs")
cor ( xx$edad, xx$peso , method = "spearman", use="pairwise.complete.obs")
cor ( xx$edad, xx$peso , method = "kendall", use="pairwise.complete.obs")

## Test del coeficiente de correlación H0: r = 0
cor.test ( xx$edad, xx$peso, method = "pearson", use="pairwise.complete.obs")


################################################################################
## Gráfico de dispersión
################################################################################

dev.new()
plot( xx$edad, xx$peso ,pch=16, xlab="Edad", ylab="Peso", col="blue")




