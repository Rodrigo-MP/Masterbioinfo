################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica
##
## Autor: Jes�s Herranz
##
## Ejercicios Sesi�n 10: An�lisis de Correlaciones
##
## Fichero de datos: Bajo peso al nacer
##
################################################################################
################################################################################

################################################################################
## Fichero Datos: Bajo peso al nacer
################################################################################

xx <- read.csv(file="/home/rodrigo/github/Masterbioinfo/R/Datos/Bajo peso al nacer.csv", sep=";")
dim(xx)
head(xx)


################################################################################
## Correlaciones
################################################################################

## Correlaci�n: Pearson, Spearman, Kendall
cor ( xx$edad, xx$peso , method = "pearson", use="pairwise.complete.obs")
cor ( xx$edad, xx$peso , method = "spearman", use="pairwise.complete.obs")
cor ( xx$edad, xx$peso , method = "kendall", use="pairwise.complete.obs")

## Test del coeficiente de correlaci�n H0: r = 0
cor.test ( xx$edad, xx$peso, method = "pearson", use="pairwise.complete.obs")


################################################################################
## Gr�fico de dispersi�n
################################################################################

dev.new()
plot( xx$edad, xx$peso ,pch=16, xlab="Edad", ylab="Peso", col="blue")
