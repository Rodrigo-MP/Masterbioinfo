################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##                             
## Ejercicios Sesión 21: Análisis de medidas repetidas
##
## Fichero de datos: evolución cirugía
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
   boxplot( sat[grupo == i] ~ tiempo[grupo == i] , data=xx,
            col=pal.col[i], xlab="Tiempo", ylab="sat",
            ylim=c(min(xx$sat,na.rm=T), max(xx$sat,na.rm=T)),
            main=paste("Evolución del sat en el Grupo =", i ))
}


###################
## Cálculo de medias y SD de cada

medias.gr.tm <- tapply ( xx$sat , list ( xx$grupo, xx$tiempo ) , mean , na.rm=T )
sd.gr.tm     <- tapply ( xx$sat , list ( xx$grupo, xx$tiempo ) , sd , na.rm=T )
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

legend(1,92, c("Gr1","Gr2","Gr3","Gr4"), pal.col , cex = 0.8)



################################################################################
## Análisis de medidas repetidas
################################################################################


###################
## Modelo con Interacción
lme.int <- lme( fixed = sat ~ tiempo * grupo , random = ~1 | id
                 , data=xx, na.action=na.omit)
anova(lme.int)
summary(lme.int)


                 