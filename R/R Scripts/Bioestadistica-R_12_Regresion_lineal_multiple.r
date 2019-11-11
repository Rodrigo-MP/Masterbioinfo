################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##
## Sesión 12: Regresión lineal múltiple                        
##
################################################################################
################################################################################

################################################################################
## Fichero Datos: cystic fibrosis
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/cystic fibrosis.csv", sep=";")
dim(xx)
head(xx)

## Variables categóricas

xx$sex <- factor(xx$sex)

################################################################################
## Regresión lineal multiple
################################################################################

## Explorando las covariables
dev.new()
pairs ( xx[ , c(2,4:11) ] )

## Explorando los modelos de regresión simples (variables 2:10)
dev.new()
par(mfrow=c(3,3))

for ( i in 2:10 )
{ lm.w <- lm ( xx$pemax ~  xx[ , i ] )
  plot ( xx[ , i ], xx$pemax, main=paste ("PEmax - ", names(xx)[i] ), 
         xlab=names(xx)[i], ylab="PEmax" ) 
  abline(lm.w)
  print ( paste ( names(xx)[i] , round( anova(lm.w)$"Pr(>F)"[1], dig=4), sep=" - " )) 
}


####################################
## Regresión lineal multiple

lm.mod  <- lm( pemax ~ age + fev + rv , data=xx)
summary(lm.mod)

## Intervalos de confianza
confint(lm.mod)

w.coef = summary(lm.mod)$coeff
w.ic   = confint(lm.mod)

for ( i in 2:nrow(w.coef) )
{
  print ( paste( round( w.coef[i,1] , 2 ) , "(" , 
                 round( w.ic[i,1] , 2 ) , "-" , round(w.ic[i,2] , 2) , ")"  ))
}

## R2
summary(lm.mod)$r.squared
summary(lm.mod)$adj.r.squared


################################################################################
## Comprobando los supuestos del modelo
################################################################################

## Gráfico de los residuos frente a los valores ajustados y los predictores 
dev.new()
par(mfrow=c(2,3))
plot(lm.mod$fit, lm.mod$res, xlab="Ajustados", ylab="Residuos", cex.lab=1.2)
plot(xx$age, lm.mod$res, xlab="Edad", ylab="Residuos", cex.lab=1.2)
plot(xx$fev, lm.mod$res, xlab="FEV", ylab="Residuos", cex.lab=1.2)
plot(xx$rv, lm.mod$res, xlab="RV", ylab="Residuos", cex.lab=1.2)

## QQPlot de los residuos 
qqnorm(lm.mod$res,ylab="Residuos", cex.lab=1.2)
qqline(lm.mod$res)

## Distancia de Cook
cook.mod <- cooks.distance(lm.mod)
max(cook.mod)
which.max(cook.mod)
plot(cook.mod, ylab="Distancia de Cook", cex.lab=1.2)


################################################################################
## Ejemplo distancia de Cook (Ejemplo regresión robusta simple) 
################################################################################

x  <- c(11,12,14,15,16,17,18,20,22,25)
y1 <- c(22,25,21,28,32,23,20,36,33,35)
y2 <- c(22,25,21,28,32,23,27,36,33,20)

lm.1  <- lm( y1 ~ x )
lm.2  <- lm( y2 ~ x )

cook.1 <- cooks.distance(lm.1)
cook.2 <- cooks.distance(lm.2)


################################################################################
## Colinealidad

cor( xx$height , xx$weight)

summary ( lm( pemax ~ height , data=xx ) )
summary ( lm( pemax ~ weight , data=xx ) )
summary ( lm( pemax ~ height + weight , data=xx ) )




