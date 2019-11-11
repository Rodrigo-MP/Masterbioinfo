################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
## 
## Sesión 11: Regresión lineal simple
##
################################################################################
################################################################################

library(rrcov)


################################################################################
## Fichero Datos: cystic fibrosis
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/cystic fibrosis.csv", sep=";")
dim(xx)
head(xx)

## Variables categóricas

xx$sex <- factor(xx$sex)

################################################################################
## Regresión lineal simple
################################################################################

####################################
## Ajuste del modelo de regresión
lm.fev  <- lm( pemax ~ fev , data=xx)
summary(lm.fev)

####################################
## Gráfico de dispersión
dev.new()
plot ( xx$fev, xx$pemax, pch=16, cex=1.5, main="PEmax frente a FEV",
       xlab="FEV", ylab="PEmax" )
abline ( lm.fev, col="red", lwd=2 )

####################################
## Intervalos de confianza
confint(lm.fev)
confint(lm.fev)[2,]

## Objetos
summary(lm.fev)$coeff
summary(lm.fev)$coeff[2,1]    ## coeficiente
summary(lm.fev)$coeff[2,4]    ## p-value
summary(lm.fev)$r.squared

paste( round(summary(lm.fev)$coeff[2,1],2) , "(" , 
       round(confint(lm.fev)[2,1],2) , "-" , round(confint(lm.fev)[2,2],2), ")" )

####################################
## Tabla ANOVA
anova(lm.fev)

####################################
## Valores ajustados y residuos
fitted(lm.fev)
resid(lm.fev)                                  


################################################################################
## Regresión Robusta: LTS
################################################################################

x  <- c(11,12,14,15,16,17,18,20,22,25)
y1 <- c(22,25,21,28,32,23,20,36,33,35)  ## Outlier en Y
y2 <- c(22,25,21,28,32,23,27,36,33,20)  ## Outlier en X

dev.new(); par(mfrow=c(1,2))

## Ejemplo 1
lm.1  <- lm( y1 ~ x )
lts.1 <- ltsReg( y1 ~ x , use.correction=F)
lm.1$coef
lts.1$coef
plot(x,y1,pch=16,cex=1.5)
abline(lm.1,col="blue", lwd=2)
abline(lts.1,col="red", lwd=2)

## Ejemplo 2
lm.2  <- lm( y2 ~ x )
lts.2 <- ltsReg( y2 ~ x , use.correction=F)
lm.2$coef
lts.2$coef
plot(x,y2,pch=16,cex=1.5)
abline(lm.2,col="blue", lwd=2)
abline(lts.2,col="red", lwd=2)


