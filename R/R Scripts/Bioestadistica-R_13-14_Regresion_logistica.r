################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##  
## Sesión 13: Regresión Logística I
## Sesión 14: Regresión Logística II
##
################################################################################
################################################################################

################################################################################
## Fichero Datos: Bajo peso al nacer
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/Bajo peso al nacer.csv", sep=";")
dim(xx)
head(xx)

## Variables categóricas

xx$raza     <- factor(xx$raza)
xx$fumador  <- factor(xx$fumador)
xx$part_pre <- factor(xx$part_pre)
xx$hta      <- factor(xx$hta)
xx$irr_urin <- factor(xx$irr_urin)

## Recodificaciones

xx$part_pre2 <- NA
xx$part_pre2 [ xx$part_pre == 0] <- 0
xx$part_pre2 [ xx$part_pre == 1] <- 1
xx$part_pre2 [ xx$part_pre == 2] <- 1
xx$part_pre2 [ xx$part_pre == 3] <- 1


################################################################################
## Modelos de Regresión Logística Univariantes
################################################################################

###############################################
## 1.- Variable continua: peso

out.1 <- glm ( bajo_pes ~ peso , data=xx, family = binomial )
summary(out.1)

## Predice probabilidades
predict ( out.1, data.frame(peso=50), type="response")
predict ( out.1, data.frame(peso=70), type="response")

## Carga los coefficientes
names(summary(out.1))
coef.1 <- summary(out.1)$coeff
coef.1

## IC95% de los Betas
confint(out.1)

## OR y IC95%
exp( coef.1[2,1] )
exp( coef.1[2,1] - 1.96 * coef.1[2,2] )
exp( coef.1[2,1] + 1.96 * coef.1[2,2] )

## Cambio de unidad de 10 kg.
exp( 10*coef.1[2,1] )


###############################################
## 2.- Variable binaria: fumador

out.2 <- glm(bajo_pes ~ fumador , data=xx, family = binomial )
summary(out.2)
coef.2 <- summary(out.2)$coeff
exp( coef.2[2,1] )
exp( coef.2[2,1] - 1.96 * coef.2[2,2] )
exp( coef.2[2,1] + 1.96 * coef.2[2,2] )

## Cambiando la categoría de referencia
out.2b <- glm(bajo_pes ~ relevel(fumador,"1") , data=xx, family = binomial )
summary(out.2b)
coef.2b <- summary(out.2b)$coeff
exp( coef.2b[2,1] )

###############################################
## 3.- Variable categórica: raza

table(xx$raza)
prop.table(table(xx$bajo_pes, xx$raza), 2)
out.3 <- glm(bajo_pes ~ raza , data=xx, family = binomial )
summary(out.3)
coef.3 <- summary(out.3)$coeff

for ( i in 2:3)
{ print ( paste ( round( exp( coef.3[i,1] ), dig=2) , 
                  " (", 
                  round( exp( coef.3[i,1] - 1.96 * coef.3[i,2] ), dig=2) , 
                  "-" ,
                  round( exp( coef.3[i,1] + 1.96 * coef.3[i,2] ), dig=2) ,
                  ")", sep ="") )
}                  

## Test Cociente del verosimilitudes para evaluar la variable globalmente
names(out.3)    
out.3$null.deviance
out.3$deviance
out.3$null.deviance - out.3$deviance

## 1º forma: ANOVA del modelo
anova(out.3, test="Chisq")

## 2º forma: quitando 1 variable
drop1(out.3, test="Chisq")


################################################################################
## Ajuste del modelo
################################################################################

## Se cargan las funciones
source ( "C://Bioestadistica con R/R Scripts/Bioestadística-R Funciones.r") 

## Modelo
out.w <- glm( bajo_pes ~ edad + peso + raza + fumador + hta + irr_urin + part_pre2 
                        , data=xx, family = binomial )

## R2
LogisticModelFit (out.w)

## Test de Hosmer-Lemeshow
library(ResourceSelection)
hoslem.test( xx$bajo_pes , fitted(out.w) , g=10 ) 




