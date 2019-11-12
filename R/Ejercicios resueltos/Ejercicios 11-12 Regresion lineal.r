################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##                           
## Ejercicios Sesión 11: Regresión lineal simple
## Ejercicios Sesión 12: Regresión lineal múltiple
##
## Fichero de datos: cystic fibrosis
##
################################################################################
################################################################################

################################################################################
## Fichero Datos: cystic fibrosis
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/cystic fibrosis.csv", sep=";")
dim(xx)
head(xx)


################################################################################
## Sesion 10: Regresión lineal simple
################################################################################

## Ajuste del modelo de regresión
lm.height  <- lm( pemax ~ height , data=xx)
summary(lm.height)

## Gráfico de dispersión
dev.new()
plot(xx$height, xx$pemax, pch=16, cex=1.5, main="PEmax frente a Height",
     xlab="Height", ylab="PEmax")
abline(lm.height, col="red", lwd=2)



################################################################################
## Sesion 11: Regresión lineal multiple
################################################################################

## Ajuste del modelo de regresión
lm.mult  <- lm( pemax ~ weight + bmp + fev + rv , data=xx)
summary(lm.mult)


## Gráfico de los residuos frente a los valores ajustados y los predictores 
dev.new()
par(mfrow=c(1,2))
plot(lm.mult$fit, lm.mult$res, xlab="Ajustados", ylab="Residuos", cex.lab=1.2)

## QQPlot de los residuos 
qqnorm(lm.mult$res,ylab="Residuos", cex.lab=1.2)
qqline(lm.mult$res)

## Distancia de Cook
cook.m <- cooks.distance(lm.mult)
max(cook.m)
which.max(cook.m)


