################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##
## Ejercicios Sesión 20: Capacidad predicitiva de un modelo. Curvas ROC
##       
## Fichero de datos: umaru
##
################################################################################
################################################################################

###############################################
## Librerías
###############################################

library(pROC)


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


###############################################
## Modelo multivariante de efectos principales
out.12 <- glm(DFREE ~ AGE + IVHX + NDRUGTX + RACE + TREAT + SITE , 
              data=xx, family = binomial ) 
summary(out.12)              

## Probabilidades ajustadas por el modelo
prob.12 <- predict.glm ( out.12, type="response") 


###############################################
## Análisis de la Curva ROC
roc.out <- roc( xx$DFREE , prob.12 )

## AUC con IC95%
out.auc  = auc ( roc.out ) 
out.auc
ci.auc (roc.out)   

## Gráfico de la curva ROC
dev.new()
plot( roc.out , legacy.axes = TRUE, col="blue" )
text ( 0.2, 0.2 , paste ( "AUC =" , round ( out.auc , 2 ) ) )

               