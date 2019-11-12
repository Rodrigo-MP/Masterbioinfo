################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica 
##
## Autor: Jes�s Herranz
##
## Ejercicios Sesi�n 20: Capacidad predicitiva de un modelo. Curvas ROC
##       
## Fichero de datos: umaru
##
################################################################################
################################################################################

###############################################
## Librer�as
###############################################

library(pROC)


################################################################################
## Fichero Datos: UMARU
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/umaru.csv", sep=";", header=TRUE)
dim(xx)
head(xx)

## Variables categ�ricas

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
## An�lisis de la Curva ROC
roc.out <- roc( xx$DFREE , prob.12 )

## AUC con IC95%
out.auc  = auc ( roc.out ) 
out.auc
ci.auc (roc.out)   

## Gr�fico de la curva ROC
dev.new()
plot( roc.out , legacy.axes = TRUE, col="blue" )
text ( 0.2, 0.2 , paste ( "AUC =" , round ( out.auc , 2 ) ) )

               