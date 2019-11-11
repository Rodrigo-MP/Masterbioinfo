################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
## 
## Sesión 08: Inferencia básica. Variables Continuas
##
################################################################################
################################################################################

library(nortest)
library(car)

################################################################################
## Fichero Datos: Bajo peso al nacer
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/Bajo peso al nacer.csv", sep=";")
dim(xx)
head(xx)

## Variables categóricas

xx$bajo_pes <- factor(xx$bajo_pes)
xx$raza     <- factor(xx$raza)
xx$fumador  <- factor(xx$fumador)
xx$part_pre <- factor(xx$part_pre)
xx$hta      <- factor(xx$hta)
xx$irr_urin <- factor(xx$irr_urin)


################################################################################
## Tests para 2 muestras
################################################################################

## Boxplot
dev.new()
boxplot ( xx$edad ~  xx$bajo_pes, col=c("red","blue"), 
          main="Edad - Bajo peso al nacer" )

## Stripchart (la opción "jitter" es porque no se sabe cuántas observaciones hay
##             en cada punto). Tiene sentido para muestras pequeñas
dev.new()
stripchart( xx$edad ~  xx$bajo_pes, method="jitter" , xlab="Edad", 
          main="Edad - Bajo peso al nacer" )          

## Contrastes de Normalidad en cada grupo
lillie.test ( xx$edad[xx$bajo_pes==0] ) 
lillie.test( xx$edad[xx$bajo_pes==1] ) 
          
shapiro.test ( xx$edad[xx$bajo_pes==0])
shapiro.test ( xx$edad[xx$bajo_pes==1])

## Test de igualdad de varianza
var.test ( xx$edad ~  xx$bajo_pes)
leveneTest( xx$edad ~  xx$bajo_pes) 
       

## Test de igualdad de medias. T de Student
t.test ( xx$edad ~  xx$bajo_pes)
t.test ( xx$edad ~  xx$bajo_pes, var.equal=T)

## Test no paramétrico de Mann-Whitney o test de Wilcoxon
wilcox.test ( xx$edad ~  xx$bajo_pes)


################################################################################
## Informe de los Tests de 2 muestras de las variables continuas
################################################################################

## Se cargan las funciones
source ( "C://Bioestadistica con R/R Scripts/Bioestadística-R Funciones.r") 


## Se abre el fichero de salida y se escribe el título y la cabecera
FileOut <- file("C://Bioestadistica con R/Temp/Bajo Peso Test Variables continuas.csv", "w")

cat ( "Tabla: Test de igualdad de Bajo peso al nacer. Variables Continuas", file=FileOut, sep="\n")
cat ( "", file=FileOut, sep="\n")
cat ( ";Grupo;Media(SD);Mediana(MAD);P(normal);P(var);P(t);P(W)", file=FileOut, sep="\n")

## Tests 2 muestras
AnalisisBinariaContinua ( xx$edad, xx$bajo_pes , FileOut, "Edad" )
AnalisisBinariaContinua ( xx$peso, xx$bajo_pes , FileOut, "Peso" )

close(FileOut)




################################################################################
## Tests para 1 muestra
################################################################################

## Contraste de Normalidad
lillie.test ( xx$edad )
shapiro.test ( xx$edad )

## T de Student
t.test ( xx$edad , mu=24 )

## Test no paramétrico de Mann-Whitney o test de Wilcoxon
wilcox.test ( xx$edad, mu=24 )



################################################################################
## Tests para muestras apareadas
################################################################################

## Datos de Trigliceridos de 16 pacientes, antes y después de una dieta

trigl.basal <- c(159,93,130,174,148,148,85,180,92,89,204,182,110,88,134,84)
trigl.final <- c(194,122,158,154,93,90,101,99,183,82,100,104,72,108,110,81)
trigl.dif <- trigl.final - trigl.basal
trigl.dif

## Boxplot
dev.new()
boxplot ( trigl.dif, col=c("red","blue"), 
          main="Diferencias en Trigliceridos" )

## Contrastes de Normalidad de las diferencias
lillie.test( trigl.dif )           
shapiro.test ( trigl.dif )

## Test de igualdad de medias para muestras pareadas. T de Student
t.test ( trigl.basal, trigl.final, paired=T )

## Test no paramétrico de Mann-Whitney o test de Wilcoxon para muestras pareadas
wilcox.test ( trigl.basal, trigl.final, paired=T )


################################################################################
## Análisis de muchas variables
################################################################################

## ALLSubset contiene 1000 variables genéticas y una que define 2 clases (última col)
xx <- read.delim( "C://Bioestadistica con R/Datos/ALLSubset.txt", sep=" ")
dim(xx)
head(names(xx))
tail(names(xx))
table( xx$mol.biol )


tt.out = t.test ( xx[ , 1 ] ~ xx$mol.biol )
tt.out
names(tt.out)
tt.out$p.value
tt.out$estimate

########################
## Análisis de TODAS las variables
p.values.tt = rep ( NA, ncol(xx)-1 ) 
means.gr    = matrix ( NA, ncol(xx)-1 , 2 ) 

for ( i in 1:(ncol(xx)-1) )
{ 
  tt.out = t.test ( xx[ , i ] ~ xx$mol.biol )
  p.values.tt [ i ] = tt.out$p.value
  means.gr    [ i , ] = tt.out$estimate                                  
}
head(p.values.tt)
head(means.gr)
 
## Exploramos los resultados
min ( p.values.tt )
col.min = which.min ( p.values.tt )
names(xx) [ col.min ]
length ( p.values.tt [ p.values.tt < 0.001 ] )
col.001 = which ( p.values.tt < 0.001 )
col.001 


