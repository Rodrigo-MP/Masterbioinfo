################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
## 
## Ejercicios Sesión 02: Variables y objetos de R
##
################################################################################
################################################################################

## Generar

a <- c( 12, 24, 35, 22, 55, 18, 26, 47, 28, 12 )

ind.par<-seq(2,10, by=2)
ind.impar<-seq(1,10, by=2)
ind.par
ind.impar

b <- a [ind.par]
c <- a [ind.impar]
b
c

d <- a [ a > 30 ]
d


## Generar ek texto "OR=1.33, IC95%:1.09 – 1.96" 

OR=1.33
OR_L=1.09
OR_U=1.96

paste ( "OR=", OR, ", IC95%:", OR_L, " - " , OR_U , sep="")


## Matriz

a <- matrix ( 1:90, 30, 3 )

b <- a [ 1:10, 2 ]
length(b)
mean(b)

