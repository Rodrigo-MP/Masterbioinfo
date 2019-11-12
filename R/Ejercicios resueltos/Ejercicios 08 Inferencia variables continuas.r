################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##  
## Ejercicios Sesión 08: Inferencia básica con variables continuas
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
## Tests para 2 muestras
################################################################################

## Boxplots
dev.new()
par(mfrow=c(2,2))
boxplot ( xx$AGE ~  xx$DFREE, col=c("red","blue"), 
          main="Edad - Recaida Consumo de drogas" )
boxplot ( xx$BECK ~  xx$DFREE, col=c("red","blue"), 
          main="BECK score - Recaida Consumo de drogas" )
boxplot ( xx$NDRUGTX ~  xx$DFREE, col=c("red","blue"), 
          main="Consumo previo - Recaida Consumo de drogas" )          

      
## Contrastes de Normalidad en cada grupo
lillie.test( xx$BECK[xx$DFREE==0] ) 
lillie.test( xx$BECK[xx$DFREE==1] ) 
          
shapiro.test ( xx$BECK[xx$DFREE==0])
shapiro.test ( xx$BECK[xx$DFREE==1])

## Test de igualdad de varianza
var.test ( xx$BECK ~  xx$DFREE)

## Test de igualdad de medias. T de Student con varianzas iguales
t.test ( xx$BECK ~  xx$DFREE, var.equal=T)

## Test no paramétrico de Mann-Whitney o test de Wilcoxon
wilcox.test ( xx$BECK ~  xx$DFREE)


################################################################################
## Informe de los Tests de 2 muestras de las variables continuas
################################################################################

## Se cargan las funciones
source ( "C://Bioestadistica con R/R Scripts/Estadística-R Funciones.r")


## Se abre el fichero de salida y se escribe el título y la cabecera
FileOut <- file("C://Bioestadistica con R/Temp/Umaru Test Variables continuas.csv", "w")

cat ( "Tabla: Test de igualdad de Recaida Consumo de drogas. Variables Continuas", file=FileOut, sep="\n")
cat ( "", file=FileOut, sep="\n")
cat ( ";;Media(SD);Mediana(MAD);P(normal);P(var);P(t);P(W)", file=FileOut, sep="\n")

## Tests 2 muestras
AnalisisBinariaContinua ( xx$BECK, xx$DFREE , FileOut, "Score BECK" )
AnalisisBinariaContinua ( xx$AGE, xx$DFREE , FileOut, "Edad" )
AnalisisBinariaContinua ( xx$NDRUGTX, xx$DFREE , FileOut, "Consumo previo" )

close(FileOut)




