################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##      
## Sesión 06: Programación y funciones en R
##
################################################################################
################################################################################


##########################################
## Ejecución condicional: orden if

a = 4
if ( a > 6 ) b = 5 else b = 8
b

if ( a > 6 ) { 
  b = 7 
} else { 
  b = 3 
}
b

a > 6
  

##########################################
## Bucle con while

x = c ( 4, 10, 11, 13, 15, 16, 17, 18, 19, 20, 22, 24, 25, 27, 29, 30, 31 )

i <- 1
while ( x[i] < 20 )
{
  print ( x[i] )
  i <- i + 1
} 

##########################################
## Bucle con repeat

x

i <- 1
repeat 
{
  if ( x[i] >= 20 )
  { break }
  else
  { 
    print ( x[i] )
    i <- i + 1
  }  
} 

##########################################
## Bucle con for

x

for ( i in 1:length(x) ) 
{
  if ( x [i] >= 20 )
  { break }
  else
  { print ( x[i] ) }  
} 

for ( i in 1:length(x) ) 
{ if ( x [i] < 15 ) print ( x[i] ** 2 ) }

## Cálculo vectorial
x[ x < 15 ] ** 2


################################################################################
## Fichero Datos: Bajo peso al nacer
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/Bajo peso al nacer.csv", sep=";")
dim(xx)
head(xx)


##########################################
## Bucle con for

## Nombres de las variables
names(xx)

## Variables categóricas

xx$bajo_pes <- factor(xx$bajo_pes)
xx$raza     <- factor(xx$raza)
xx$fumador  <- factor(xx$fumador)
xx$part_pre <- factor(xx$part_pre)
xx$hta      <- factor(xx$hta)
xx$irr_urin <- factor(xx$irr_urin)

## Detectamos las variables que son categóricas

for ( i in 3:10 )
{ 
   if ( is.factor( xx[ , i ] ) == TRUE  ) 
        print ("es factor") else print ("no es factor") 
}

## Análisis Descriptivo de las variables independientes
## - Si X es un factor, mostramos la tabla
## - SI X es una variable continua, mostramos la media y la SD

for ( i in 3:10)
{ 
   print ( names(xx)[i] )
   if ( is.factor( xx[, i ] ) == TRUE  ) 
   {  
     print ( table (xx[, i ]) ) 
   }
   else 
   { 
     print ( paste ( "media(SD) =", round( mean(xx[ , i]), dig=2) , 
                              "(",  round( sd(xx[ ,i]),dig=2 ), ")" ))  
   } 
}


################################################################################
## Ejemplo de funciones
################################################################################

## Función que calcula las medidas resumen de una variable continua 

AnalisisContinua <- function ( var )
{
  if ( is.factor(var) == TRUE )
  { stop ( "La variable no es continua" ) }
  
  ## Medidas de tendencia central
  mean = mean (var, na.rm=TRUE)
  median = median(var, na.rm=TRUE)
  mean.trim.10 = mean(var, trim=0.10)
  
  ## Medidas de Dispersión - Variabilidad
  sd = sd (var, na.rm=TRUE)
  iqr = IQR (var, na.rm=TRUE)   
  mad = mad (var, na.rm=TRUE) 

  list ( mean=mean, median=median, mean.trim.10 = mean.trim.10, 
         sd=sd, iqr=iqr, mad=mad)
}

summ.edad <- AnalisisContinua (xx$edad)
summ.raza <- AnalisisContinua (xx$raza)
summ.peso <- AnalisisContinua (xx$peso)

summ.peso

class(summ.peso)
names(summ.peso)

summ.peso$mean
summ.peso$sd

################################################################################
## Funciones de R y Objetos
################################################################################

x1 = c( 12,34,23,34,73,18,36,23,55,62,38,76 )
x2 = c(  0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0 )
y =  c( 16,43,22,45,66,32,47,12,27,72,45,33 )

## Regresión Lineal
out = lm ( y ~ x1 + x2 )
out
class(out)
names(out)
out$coeff
out$residuals

summary(out)
summary(out)$coeff
summary(out)$coeff[ , 4]   ## P-values


################################################################################
## Funciones de la familia "apply"
################################################################################

## apply
x <- matrix(1:10, 2, 5)
x
apply(x,1,mean)

length(apply(x,1,mean))
is.vector(apply(x,1,mean))
apply(x,1,mean)[1]
apply(x,1,mean)[2]


## tapply                                                  
a <- factor( c(1,2,1,1,1,1,2,2,1,1))
a
x <- c (2.3, 4.4, 5.2, 3.3, 2.1, 3, 2, 3.3, 4.1, 2.9)
tapply(x, a, mean)

## tapply con 2 vectores definiendo los niveles
b <- c("H","H","H","H","H","M","M","M","M","M")
tapply(x, list(a, b) , mean)

                                                 
## lapply
list3 <- list( a=1:5, b=c(3,NA,2), c=c(FALSE,TRUE,FALSE,TRUE,TRUE))
lapply(list3, mean)


