################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##              
## Sesión 20: Modelos Predictivos. Análisis de Curvas ROC
##
################################################################################
################################################################################

################################################################################
################################################################################
## 1.- REGRESION LOGISTICA                           
################################################################################
################################################################################

###############################################
## Librerías
###############################################

library(gplots)
library(pROC)

###############################################
## Fichero Datos: Bajo peso al nacer
###############################################

xx <- read.csv(file="C:/Bioestadistica con R/Datos/Bajo peso al nacer.csv", sep=";")
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

xx$peso_gr <- NA
xx$peso_gr [ xx$peso < 49.5 ] <- 1
xx$peso_gr [ xx$peso > 49.5 ] <- 0


###############################################
## Modelo Final de Regresión Logística 
###############################################

out.7 <- glm( bajo_pes ~ edad + peso_gr + raza + fumador + hta + irr_urin + part_pre2  
              , data=xx, family = binomial ) 
summary(out.7)

out.8 <- glm( bajo_pes ~ edad + peso_gr + raza + fumador + hta + irr_urin + part_pre2 +
              edad*peso_gr + peso_gr*fumador , data=xx, family = binomial )
summary(out.8)  

## Probabilidades ajustadas por el modelo
prob.7 <- predict.glm ( out.7, type="response") 
prob.8 <- predict.glm ( out.8, type="response") 


## Análisis de la Curva ROC
roc.out.7 <- roc( xx$bajo_pes , prob.7 )
roc.out.8 <- roc( xx$bajo_pes , prob.8 )

## AUC con IC95%
out.auc.7  = auc (roc.out.7) 
out.auc.7
w.auc.7 = out.auc.7 [1]
w.auc.7

out.auc.8  = auc (roc.out.8) 
w.auc.8 = out.auc.8 [1]
w.auc.8

## IC95%
out.ci.7   = ci.auc (roc.out.7)   
out.ci.7
out.ci.8   = ci.auc (roc.out.8)   
out.ci.8

w.ci.7  = c ( out.ci.7 [1], out.ci.7 [3] )
w.ci.7


## Gráfico de la curva ROC
dev.new()
plot( roc.out.7 , legacy.axes = TRUE, col="blue" )
plot( roc.out.8 , legacy.axes = TRUE, col="red", add=T )
## Leyenda del gráfico
label.7 <- paste("AUC-Mod1 =", round(w.auc.7, dig=3))
label.8 <- paste("AUC-Mod2 =", round(w.auc.8, dig=3))
legend(0.6,0.2, c(label.7, label.8), c("blue","red"),  cex = 1.1)


#################################
## Indice de Youden

## Extraer la Sensibilidad y Especificidad
espe <- coords ( roc.out.8, roc.out.8$thresholds, "thr", "sp")
sens <- coords ( roc.out.8, roc.out.8$thresholds, "thr", "se")

class(sens)
dim(sens)
sens[1,1:12]


## Indice de Youden
ind.youden <- sens + espe - 1
dev.new()
plot( roc.out.8$thresholds,  ind.youden, type="l" 
     , xlab="Probabilidades predichas", ylab="Indice de Youden")

## Maximo del Indice de Youden
max(ind.youden)
## Posicion donde está el máximo
which.max(ind.youden)
## Probabilidad predicha que da el máximo
roc.out.8$thresholds[55]
sens[55]
espe[55]


################################################################################
################################################################################
## 2.- REGRESION COX
################################################################################
################################################################################

###############################################
## Librerías
###############################################

library(gplots)
library(splines)
library(survival)
library(survivalROC)
library(rms)


###############################################
## Fichero Datos: whas500
###############################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/whas500.csv", sep=";")
dim(xx)
head(xx)

###############################################
## Modelo Final de Regresión de Cox 
###############################################

cox5 <- coxph ( Surv(lenfol,fstat) ~ age + hr + diasbp + bmi + gender + chf 
                                        + age*gender, data=xx)
summary(cox5)


## Definición del Risk Score (predcitor lineal)
risk.score <- cox5$linear.predictors

###############################################
## Percentiles de Riesgo 
###############################################

## Percentiles de riesgo 0.10, 0.25, 0.5, 0.75, 0.90
risk_quant<-quantile(risk.score, c(0.10, 0.25, 0.5, 0.75, 0.90 ))
risk_quant

## Cálculo de la supervivencia

S0t<-exp(-basehaz(cox5)[1])
times<-basehaz(cox5)[2]

## Gráfica de los percentiles de riesgo
dev.new()
plot(times[,1] , S0t[,1]**exp(risk_quant[1]),type="s", 
     main="Supervivencia estimadas para percentiles de riesgo", 
     xlab="Tiempo", ylab="Supervivencia" ,ylim=c(0,1))
lines(times[,1] , S0t[,1]**exp(risk_quant[2]),type="s")
lines(times[,1] , S0t[,1]**exp(risk_quant[3]),type="s")
lines(times[,1] , S0t[,1]**exp(risk_quant[4]),type="s")
lines(times[,1] , S0t[,1]**exp(risk_quant[5]),type="s")


###############################################
## C-index   
###############################################

## C-index (con el risk score cambiado de signo)
rcorr.cens ( - risk.score , Surv(xx$lenfol,xx$fstat) )["C Index"]


###############################################
## Curva ROC: prediciendo Supervivencia a 5 años   
###############################################

## Análisis de curva ROC

nobs <- nrow(xx)
cutoff <- 5 * 365.25 ## 5 AÑOS

surv.ROC5 = survivalROC( xx$lenfol, xx$fstat, risk.score,
                         predict.time = cutoff, span = 0.25*nobs^(-0.20) )

## Curva ROC
dev.new()
plot(surv.ROC5$FP, surv.ROC5$TP, type="l", xlim=c(0,1), ylim=c(0,1),
      col="red", lwd=3, xlab="1-Especifidad", ylab="Sensibilidad",
      main="Curva ROC prediciendo supervivencia a 5 años")

abline(0,1)

## AUC
surv.ROC5$AUC


