################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##                             
## Ejercicios Sesión 22: Comparaciones Múltiples
##
## Fichero de datos: Virco
##
################################################################################
################################################################################

library(qvalue)

################################################################################
## Fichero Datos: Virco
################################################################################

xx <- read.csv("C://Bioestadistica con R/Datos/Virco_data.csv", sep=";")
dim(xx)

xx$sens.NFV = factor(xx$sens.NFV)


################################################################################
## Comparaciones Múltiples
################################################################################

## Función con el Test de Fisher
func.fisher = function ( w.col ) 
{ tt = table ( w.col , xx$sens.NFV ) 
  return ( fisher.test ( tt )$p.value )
} 

## Test de Fisher para todas las columnas
p.val = apply ( xx[ , - 90 ] , 2 , func.fisher )

## Chequeando los p.values sin ajustar
sum ( p.val < 0.05 )
dev.new() ; hist ( p.val )

## Comparaciones Múltiples. Bonferroni
p.adj.bonf = p.adjust ( p.val , method = "bonferroni" )
sum ( p.adj.bonf  < 0.05 )

## Comparaciones Múltiples. FDR - Benjamini Hochberg
p.adj.BH = p.adjust ( p.val , method = "BH" )
sum ( p.adj.BH  < 0.05 )

## q-values
qval.out = qvalue ( p.val  , fdr.level = 0.05 )
table ( qval.out$significant )





    