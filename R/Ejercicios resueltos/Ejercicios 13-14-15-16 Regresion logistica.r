################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##
## Ejercicios Sesión 13: Regresión Logística I
## Ejercicios Sesión 14: Regresión Logística II
## Ejercicios Sesión 15: Variables de confusión e Interacción
## Ejercicios Sesión 16: Construcción de un modelo de regresión
##
## Fichero de datos: umaru
##
################################################################################
################################################################################

################################################################################
## Fichero Datos: UMARU
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/umaru.csv", sep=";", header=TRUE)
dim(xx)
head(xx)

## Variables categóricas

xx$IVHX  <- factor(xx$IVHX)
xx$RACE  <- factor(xx$RACE)
xx$TREAT <- factor(xx$TREAT)
xx$SITE  <- factor(xx$SITE)
xx$DFREE <- factor(xx$DFREE)

## Tablas

table(xx$IVHX)
table(xx$RACE)
table(xx$TREAT)
table(xx$SITE)
table(xx$DFREE)


################################################################################
## Sesion 13: Regresión Logística I
################################################################################


###############################################
## - Variable AGE

out.1 <- glm(DFREE ~ AGE , data=xx, family = binomial )
summary(out.1)
coef.1 <- summary(out.1)$coeff

## OR y IC95%
exp( coef.1[2,1] )
exp( coef.1[2,1] - 1.96 * coef.1[2,2] )
exp( coef.1[2,1] + 1.96 * coef.1[2,2] )

## Predice probabilidades
predict ( out.1, data.frame(AGE=30), type="response")

## Cambio de unidad de 5 Años
exp( 5*coef.1[2,1] )



################################################################################
## Sesion 14: Regresión Logística II
################################################################################


###############################################
## - Variable TREAT

out.2 <- glm(DFREE ~ TREAT , data=xx, family = binomial )
summary(out.2)
coef.2 <- summary(out.2)$coeff

## OR y IC95%
exp( coef.2[2,1] )
exp( coef.2[2,1] - 1.96 * coef.2[2,2] )
exp( coef.2[2,1] + 1.96 * coef.2[2,2] )


###############################################
## - Variable IVHX

out.3 <- glm(DFREE ~ IVHX , data=xx, family = binomial )
summary(out.3)
coef.3 <- summary(out.3)$coeff
coef.3

## OR y IC95%
exp( coef.3[2,1] )
exp( coef.3[2,1] - 1.96 * coef.3[2,2] )
exp( coef.3[2,1] + 1.96 * coef.3[2,2] )

exp( coef.3[3,1] )
exp( coef.3[3,1] - 1.96 * coef.3[3,2] )
exp( coef.3[3,1] + 1.96 * coef.3[3,2] )

## Significación global de la variable
anova(out.3, test="Chisq")



################################################################################
## Sesion 15: Variables de confusión e Interacción
################################################################################


###############################################
## - Variable de confusión

summary(glm(DFREE ~ TREAT , data=xx, family = binomial ))
summary(glm(DFREE ~ TREAT + RACE , data=xx, family = binomial ))
(0.4372 - 0.4095 ) / 0.4095 


###############################################
## - Interacción

out.5 <- glm(DFREE ~ RACE*SITE , data=xx, family = binomial )
summary(out.5)


## Interpretación, construyendo una nueva variable

table(xx$RACE, xx$SITE)

xx$RACE_SITE <- 0
xx$RACE_SITE[ xx$RACE == 0 & xx$SITE == 1 ] <- 1
xx$RACE_SITE[ xx$RACE == 1 & xx$SITE == 0 ] <- 2
xx$RACE_SITE[ xx$RACE == 1 & xx$SITE == 1 ] <- 3
xx$RACE_SITE <- factor( xx$RACE_SITE )

table(xx$RACE_SITE )

out.6 <- glm(DFREE ~ RACE_SITE , data=xx, family = binomial )
summary(out.6)
coef.6 <- summary(out.6)$coeff

for ( i in 2:4)
{ print ( paste ( round( exp( coef.6[i,1] ), dig=2) , 
                  " (", 
                  round( exp( coef.6[i,1] - 1.96 * coef.6[i,2] ), dig=2) , 
                  "-" ,
                  round( exp( coef.6[i,1] + 1.96 * coef.6[i,2] ), dig=2) ,
                  ")", sep ="") )
}                  



################################################################################
## Sesion 16: Construcción de un modelo de regresión
################################################################################


###############################################
## Modelo multivariante completo
out.11 <- glm(DFREE ~ AGE + BECK + IVHX + NDRUGTX + RACE + TREAT + SITE , 
              data=xx, family = binomial )
summary(out.11)


###############################################
## Modelo multivariante de efectos principales
out.12 <- glm(DFREE ~ AGE + IVHX + NDRUGTX + RACE + TREAT + SITE , 
              data=xx, family = binomial ) 
summary(out.12)              


###############################################
## Analizando la escala de la variable continua NDRUGTX

quantile(xx$NDRUGTX, , right=F)
xx$NDRUGTX_4gr<-cut(xx$NDRUGTX, breaks = c(quantile(xx$NDRUGTX)[1:4], 999)  ,
                labels=1:4  , right=F)
table(xx$NDRUGTX_4gr, xx$NDRUGTX)
table(xx$NDRUGTX_4gr)                        
                        
out.13 <- glm(DFREE ~ AGE + IVHX + NDRUGTX_4gr + RACE + TREAT + SITE , 
              data=xx, family = binomial ) 
summary(out.13)    

                   