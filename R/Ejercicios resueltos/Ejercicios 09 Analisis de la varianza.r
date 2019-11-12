################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
## 
## Ejercicios Sesión 09: Análisis de la Varianza
##
## Fichero de datos: umaru
##    
################################################################################
################################################################################

library(nortest)

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


################################################################################
## Análisis de la varianza (ANOVA de un factor)
################################################################################

## Boxplots
dev.new()
boxplot ( xx$BECK ~  xx$IVHX, col=c("red","blue","yellow"), 
          main="BECK - IVHX" )

## Medias, medianas y SD
tapply( xx$BECK, xx$IVHX, mean )
tapply( xx$BECK, xx$IVHX, median )
tapply( xx$BECK, xx$IVHX, sd )


#################
## Supuestos para aplicar el ANOVA
   
## Contrastes de Normalidad en cada grupo (Kolmogorov-Smirnov Test)
for ( i in 1:3 )
{ 
  print ( lillie.test( xx$BECK[xx$IVHX==i] ) ) 
}

## Test de igualdad de varianza
bartlett.test ( xx$BECK ~  xx$IVHX )


#################
## Test de medias

## Análisis de la varianza (ANOVA de un factor)
summary( aov ( xx$BECK ~  xx$IVHX ) )


## Test no paramétrico de Kruskal-Wallis
kruskal.test ( xx$BECK ~  xx$IVHX )
## Comparaciones múltiples, dos a dos (Wilcoxon Test)
pairwise.wilcox.test( xx$BECK , xx$IVHX , p.adj="bonferroni" )


