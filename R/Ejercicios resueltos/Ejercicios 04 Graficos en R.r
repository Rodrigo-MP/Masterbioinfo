################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##      
## Ejercicos Sesión 04: Gráficos con R
##
################################################################################
################################################################################

##############################################    
## Fichero Datos: Bajo peso al nacer
xx <- read.csv(file="C://Bioestadistica con R/Datos/Bajo peso al nacer.csv", sep=";")

## Gráfico sin parámetros
dev.new()
plot (xx$edad, xx$peso)

## Gráfico con parámetros
dev.new()
plot( xx$edad, xx$peso ,
      main="Gráfico de dispersión de peso frente a edad", 
      xlab="Edad", ylab="Peso",
      cex.main = 0.9, cex.lab=1.2,           
      col="red", pch = 16, cex = 0.8 )
                      
## Gráfico sin ejes, y la función axis()                      
dev.new()
plot (xx$edad, xx$peso, 
      main="Gráfico de dispersión de peso frente a edad", 
      xlab="Edad", ylab="Peso",
      cex.main = 0.9, cex.lab=1.2,           
      col="red", pch = 16, cex = 0.8 , 
      axes=FALSE, ann=FALSE)           

axis(1 , c(10,50))
axis(2 , c(0,150))           

