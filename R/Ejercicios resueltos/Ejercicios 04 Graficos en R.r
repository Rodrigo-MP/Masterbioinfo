################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica
##
## Autor: Jes�s Herranz
##
## Ejercicos Sesi�n 04: Gr�ficos con R
##
################################################################################
################################################################################

##############################################
## Fichero Datos: Bajo peso al nacer
xx <- read.csv(file="/home/rodrigo/github/Masterbioinfo/R/Datos/Bajo peso al nacer.csv", sep=";")

## Gr�fico sin par�metros
dev.new()
plot (xx$edad, xx$peso)

## Gr�fico con par�metros
dev.new()
plot( xx$edad, xx$peso ,
      main="Gr�fico de dispersi�n de peso frente a edad", 
      xlab="Edad", ylab="Peso",
      cex.main = 0.9, cex.lab=1.2,
      col="red", pch = 16, cex = 0.8 )

## Gr�fico sin ejes, y la funci�n axis()
dev.new()
plot (xx$edad, xx$peso,
      main="Gr�fico de dispersi�n de peso frente a edad",
      xlab="Edad", ylab="Peso",
      cex.main = 0.9, cex.lab=1.2,
      col="red", pch = 16, cex = 0.8 ,
      axes=FALSE, ann=FALSE)

axis(1 , c(10,50))
axis(2 , c(0,150))
