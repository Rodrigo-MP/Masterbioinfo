################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
## 
## Sesión 21: Análisis de medidas repetidas
##           
################################################################################
################################################################################
      
library(nlme)
                                  
################################################################################
## Fichero Datos: Evolución después de cirugía
################################################################################

xx <- read.csv(file="C:/Bioestadistica con R/Datos/evolución cirugía.csv", 
               header=T, sep=";")
dim(xx)
head(xx)

xx$tiempo <- factor(xx$tiempo)
xx$grupo <- factor(xx$grupo)


################################################################################
## Análisis Descriptivo
################################################################################

         
###################
## Boxplots de cada Grupo
pal.col <- c("red", "blue", "green", "yellow" ) ## paleta de colores usados

dev.new()
par (mfrow=c(2,2))
for ( i in 1:length(table(xx$grupo)))
{
   boxplot( pvc[grupo == i] ~ tiempo[grupo == i] , data=xx,
            col=pal.col[i], xlab="Tiempo", ylab="pvc",
            ylim=c(min(xx$pvc,na.rm=T), max(xx$pvc,na.rm=T)),
            main=paste("Evolución del PVC en el Grupo =", i ))
}


###################
## Cálculo de medias y SD de cada Grupo en cada instante del Tiempo

medias.gr.tm <- tapply ( xx$pvc , list ( xx$grupo, xx$tiempo ) , mean , na.rm=T )
sd.gr.tm     <- tapply ( xx$pvc , list ( xx$grupo, xx$tiempo ) , sd , na.rm=T )

medias.gr.tm
sd.gr.tm


###################
## Gráfico de Medias
dev.new()
plot( names(table(xx$tiempo)), medias.gr.tm[1, ],  type="l", lwd=3,
      ylim = c( min(medias.gr.tm), max(medias.gr.tm)),
      col=pal.col[1] , xlab="Tiempo", ylab="Medias", xaxp = c(1,4,3) )

for ( i in 2:4 )
{ lines( names(table(xx$tiempo)), medias.gr.tm[i, ], lwd=3, col=pal.col[i] )}

legend(3.7,8.5, c("Gr1","Gr2","Gr3","Gr4"), pal.col , cex = 0.8)



################################################################################
## Análisis de medidas repetidas
################################################################################

###################
## Modelo con efectos principales
lme.main <- lme( fixed = pvc ~ tiempo + grupo , random = ~1 | id
                 , data=xx, na.action=na.omit)
anova(lme.main)
summary(lme.main)


###################
## Modelo con Interacción
lme.int <- lme( fixed = pvc ~ tiempo * grupo , random = ~1 | id
                 , data=xx, na.action=na.omit)
anova(lme.int)
summary(lme.int)


################################################################################
## Test de Friedman
################################################################################

################################################################################
## Fichero Datos: Pico de flujo expiratorio

yy <- read.csv(file="C:/Bioestadistica con R/Datos/peak.csv", header=T, sep=";")
dim(yy)
head(yy)

#########################################
## Preparar los datos y descriptivo
id <- rep ( yy$subject, 4) 
medida <- c ( rep ( 1, nrow(yy) ) , rep ( 2, nrow(yy) ) , rep ( 3, nrow(yy) ) , rep ( 4, nrow(yy) ) )
peak <- c ( yy$wpfm_1, yy$wpfm_2, yy$mwm_1, yy$mwm_2 )
id
medida
peak

## Boxplot
dev.new()
boxplot( peak ~ medida )

## Normalidad
shapiro.test ( yy$wpfm_1 )
shapiro.test ( yy$wpfm_2 )
shapiro.test ( yy$mwm_1 )
shapiro.test ( yy$mwm_2 )

## Test de igualdad de varianza
bartlett.test ( peak ~  medida )

## Test basado en modelos lineales mixtos
anova( lme( fixed = peak ~ medida , random = ~1 | id ) )

## Test de Friedman
friedman.test(peak, medida, id)

## Test de Friedman. Alternativa para ejecutarlo
all.peak <- cbind ( yy$wpfm_1, yy$wpfm_2, yy$mwm_1, yy$mwm_2 )
friedman.test(all.peak)


