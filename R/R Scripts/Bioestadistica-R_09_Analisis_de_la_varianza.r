################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica
##
## Autor: Jes�s Herranz
##
## Sesi�n 09: An�lisis de la Varianza
##
################################################################################
################################################################################

library(nortest)

################################################################################
## Fichero Datos: Bajo peso al nacer
################################################################################

xx <- read.csv(file="/home/rodrigo/github/Masterbioinfo/R/Datos/Bajo peso al nacer.csv", sep=";")
dim(xx)
head(xx)

## Variables categ�ricas

xx$bajo_pes <- factor(xx$bajo_pes)
xx$raza     <- factor(xx$raza)
xx$fumador  <- factor(xx$fumador)
xx$part_pre <- factor(xx$part_pre)
xx$hta      <- factor(xx$hta)
xx$irr_urin <- factor(xx$irr_urin)


################################################################################
## An�lisis de la varianza (ANOVA de un factor)
################################################################################

## Boxplots
dev.new()
boxplot ( xx$peso ~  xx$raza, col=c("red","blue","yellow"),
          main="Peso - Raza" )

## Medias, medianas y SD
tapply( xx$peso, xx$raza, mean )
tapply( xx$peso, xx$raza, median )
tapply( xx$peso, xx$raza, sd )


#################
## Supuestos para aplicar el ANOVA

## Contrastes de Normalidad en cada grupo (Kolmogorov-Smirnov Test)
for ( i in 1:3 )
{
  print ( lillie.test( xx$peso[xx$raza==i] ) )
}

## Test de igualdad de varianza
bartlett.test ( xx$peso ~  xx$raza )


#################
## Test de medias

## An�lisis de la varianza (ANOVA de un factor)
summary( aov ( xx$peso ~  xx$raza ) )
## Comparaciones m�ltiples, dos a dos
pairwise.t.test( xx$peso , xx$raza , p.adj="bonferroni" )


## An�lisis de la varianza con varianzas distintas
oneway.test ( xx$peso ~  xx$raza )
## Comparaciones m�ltiples, dos a dos
pairwise.t.test( xx$peso , xx$raza , p.adj="bonferroni", pool.sd=F )


## Test no param�trico de Kruskal-Wallis
kruskal.test ( xx$peso ~  xx$raza )
## Comparaciones m�ltiples, dos a dos (Wilcoxon Test)
pairwise.wilcox.test( xx$peso , xx$raza , p.adj="bonferroni" )


################################################################################
## Modelo factorial. ANOVA con 2 factores
################################################################################

## Modelo de efectos principales
summary( aov ( xx$peso ~  xx$raza + xx$hta ) )

## Modelos con interacci�n
summary( aov ( xx$peso ~  xx$raza * xx$hta ) )

## Gr�fico de la interacci�n
dev.new()
interaction.plot ( xx$hta,xx$raza, xx$peso , lwd=3 )
