################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica
##
## Autor: Jes�s Herranz
##
## Ejercicios Sesi�n 09: An�lisis de la Varianza
##
## Fichero de datos: umaru
##
################################################################################
################################################################################

library(nortest)

################################################################################
## Fichero Datos: UMARU
################################################################################

xx <- read.csv(file="/home/rodrigo/github/Masterbioinfo/R/Datos/umaru.csv", sep=";", header=TRUE)
dim(xx)
head(xx)

## Variables categ�ricas

xx$IVHX  <- factor(xx$IVHX)
xx$RACE  <- factor(xx$RACE)
xx$TREAT <- factor(xx$TREAT)
xx$SITE  <- factor(xx$SITE)
xx$DFREE <- factor(xx$DFREE)


################################################################################
## An�lisis de la varianza (ANOVA de un factor)
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
for ( i in 1:3 ) #Usar vector si no son números --> for (i in c("B","N","O"))
{
  print ( lillie.test( xx$BECK[xx$IVHX==i] ) )
}

## Test de igualdad de varianza
bartlett.test ( xx$BECK ~  xx$IVHX )


#################
## Test de medias

## An�lisis de la varianza (ANOVA de un factor)
summary( aov ( xx$BECK ~  xx$IVHX ) )


## Test no param�trico de Kruskal-Wallis
kruskal.test ( xx$BECK ~  xx$IVHX )
## Comparaciones m�ltiples, dos a dos (Wilcoxon Test)
pairwise.wilcox.test( xx$BECK , xx$IVHX , p.adj="bonferroni" )
