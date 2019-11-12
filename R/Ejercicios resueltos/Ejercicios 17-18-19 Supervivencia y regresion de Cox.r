################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##
## Ejercicios Sesión 17: Análisis de Supervivencia 
## Ejercicios Sesión 18: Regresión de Cox I
## Ejercicios Sesión 19: Regresión de Cox II
##
## Fichero de datos: actg320
##
################################################################################
################################################################################

################################################################################
## Librerías
################################################################################

library(survival)

################################################################################
## Fichero Datos: actg320
################################################################################

xx <- read.table(file="C://Bioestadistica con R/Datos/actg320.csv",
                 header=TRUE, sep=";")

dim(xx)
head(xx)

table(xx$censor) ## variable status



################################################################################
## Sesion 16: Análisis de Supervivencia
################################################################################

###################################
## Supervivencia GLOBAL  


## KM estimator 
surv.all <- survfit ( Surv( time, censor ) ~ 1 , data=xx )
summary( surv.all )

## Mediana
surv.all 

###################################
## Curva de Supervivencia. KM plot
dev.new()
plot(surv.all, main="Estimador de Kaplan-Meier",
               xlab="Tiempo", ylab="Porcentaje de Supervivencia",
               conf.int=F, mark.time=F, lwd=2) 
      

###################################
## Comparación de curvas de supervivencia por tratamiento

surv.tx <- survfit ( Surv( time, censor ) ~ tx,  data=xx )

## Test log-rank
logrank.test <- survdiff ( Surv( time, censor ) ~ tx,  data=xx )
logrank.test


## Curvas KM por tx
dev.new()
plot(surv.tx, main="Estimadores de KM por Tratamiento", 
            xlab="Tiempo", ylab="Porcentaje de Supervivencia",
            conf.int=F, col=c("blue","red"), mark.time=F, lwd=2)

legend(20,0.10, c("Tx standard","Tx 3 med."), c("blue","red"),  cex = 0.8)




################################################################################
## Sesion 17: Regresión de Cox Univariante
################################################################################

##########################
## Variable binaria
## HR<1, el nuevo tx es protector, mejor supervivencia

cox1 <- coxph ( Surv(time,censor) ~ tx, data=xx )
summary(cox1)


##########################
## Variable continua

cox2 <- coxph ( Surv(time,censor) ~ cd4, data=xx )
summary(cox2)

##########################
## Variable categórica

quantile(xx$cd4, , right=F)
xx$cd4_4gr <- cut(xx$cd4, breaks = c(quantile(xx$cd4)[1:4], 999)  ,
                labels=1:4  , right=F)
table(xx$cd4_4gr)
sum(table(xx$cd4_4gr))

cox3 <- coxph ( Surv(time,censor) ~ cd4_4gr, data=xx )
summary(cox3)




################################################################################
## Sesion 18: Regresión de Cox Multivariante
################################################################################


##########################
## Modelo efectos principales

cox4 <- coxph ( Surv(time,censor) ~ tx + age + sex + cd4 + priorzdv, data=xx)
summary(cox4)

cox5 <- coxph ( Surv(time,censor) ~ tx + age + cd4, data=xx )
summary(cox5)


##########################
## Modelo con Interacción

coxph ( Surv(time,censor) ~ tx + age + cd4 + tx*age , data=xx )
coxph ( Surv(time,censor) ~ tx + age + cd4 + tx*cd4 , data=xx )
coxph ( Surv(time,censor) ~ tx + age + cd4 + age*cd4 , data=xx )


##########################
## Supuesto de Riesgos Proporcionales

cox.zph(cox5)
dev.new()
par(mfrow=c(2,2))
for ( i in 1:3)
{ plot(cox.zph(cox5), var=i) }


