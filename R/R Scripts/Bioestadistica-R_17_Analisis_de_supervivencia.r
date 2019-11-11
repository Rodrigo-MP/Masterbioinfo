################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica 
##
## Autor: Jes�s Herranz
##   
## Sesi�n 17: An�lisis de Supervivencia       
##
################################################################################
################################################################################

################################################################################
## Librer�as
################################################################################

library(survival)

################################################################################
## Fichero Datos: whas500
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/whas500.csv", sep=";")
dim(xx)
head(xx)


################################################################################
## Supervivencia GLOBAL
################################################################################

## Funci�n que "empaqueta" los tiempos y eventos, y se usa para los an�lisis
xx.surv <- Surv(xx$lenfol,xx$fstat)

## KM estimator 
surv.all <- survfit ( Surv(lenfol, fstat) ~ 1 , data=xx )
summary( surv.all )

## Mediana
surv.all 

###################################
## Curva de Supervivencia
dev.new()
plot(surv.all, main="Estimador de Kaplan-Meier") 
      

## Supervivencia cada a�o. Tablas de vida 
summary( surv.all, time=seq(0,3000,365))


################################################################################
## Comparaci�n de curvas de supervivencia
################################################################################

surv.sexo <- survfit ( Surv(lenfol/365, fstat) ~ gender,  data=xx )

surv.sexo  ##median
summary(surv.sexo)

## Test log-rank
logrank.test <- survdiff ( Surv(lenfol/365,fstat) ~ gender, data=xx)
logrank.test

## Test Peto-Peto (modificaci�n del de Wilcoxon)
survdiff ( Surv(lenfol/365,fstat) ~ gender, data=xx, rho=1)
                                
## Curvas KM por sexo 
dev.new()
plot ( surv.sexo, main="Estimadores de KM por sexo", 
       xlab="Tiempo", ylab="Supervivencia",
       conf.int=F, col=c("blue","red"), mark.time=F, lwd=2)

legend(0.5,0.10, c("Hombre","Mujer"), c("blue","red"),  cex = 0.8)

text( 3,0.05, cex=0.8, paste("P(log-rank)=",
        round( 1-pchisq(logrank.test$chisq, length(logrank.test$obs)-1 ), dig=4) ) )


