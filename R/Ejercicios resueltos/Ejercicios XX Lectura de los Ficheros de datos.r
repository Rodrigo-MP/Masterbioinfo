################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##
## Lectura de todos los Ficheros de Datos del Curso
##
################################################################################
################################################################################

## Se cargan las funciones
source ( "C://Bioestadistica con R/R Scripts/Estadística-R Funciones.r")

################################################################################  
## Fichero Datos: Bajo peso al nacer
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/Bajo peso al nacer.csv", sep=";")
dim(xx)

## Variables categóricas

xx$raza     <- factor(xx$raza)
xx$fumador  <- factor(xx$fumador)
xx$part_pre <- factor(xx$part_pre)
xx$hta      <- factor(xx$hta)
xx$irr_urin <- factor(xx$irr_urin)


################################################################################
## Fichero Datos: UMARU
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/umaru.csv", sep=";", header=TRUE)
dim(xx)

## Variables categóricas
 
xx$IVHX  <- factor(xx$IVHX)
xx$RACE  <- factor(xx$RACE)
xx$TREAT <- factor(xx$TREAT)
xx$SITE  <- factor(xx$SITE)
xx$DFREE <- factor(xx$DFREE)


################################################################################
## Fichero Datos: actg320
################################################################################

xx <- read.table(file="C://Bioestadistica con R/Datos/actg320.csv",
                 header=TRUE, sep=";")
dim(xx)


################################################################################
## Fichero Datos: cystic fibrosis
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/cystic fibrosis.csv", sep=";")
dim(xx)
  
################################################################################
## Fichero Datos: Evolución después de cirugía
################################################################################

xx <- read.csv(file="C:/Bioestadistica con R/Datos/evolución cirugía.csv", 
               header=T, sep=";")

dim(xx)

################################################################################
## Fichero Datos para importar
################################################################################

f1 <- read.csv(file="C://Bioestadistica con R/Ficheros para importar/Ejemplo 7.csv",
                 header=T, sep=";" )
dim(f1)

f2 <- read.csv(file="C://Bioestadistica con R/Ficheros para importar/Ejemplo 8.csv",
                 header=T, sep=";" )
dim(f2)


################################################################################
## Fichero Datos: Virco
################################################################################

xx <- read.csv("C://Bioestadistica con R/Datos/Virco_data.csv", sep=";")
dim(xx)

xx$sens.NFV = factor(xx$sens.NFV)


################################################################################
## Fichero Datos: ALLSubset
################################################################################

xx <- read.delim( "C://Bioestadistica con R/Datos/ALLSubset.txt", sep=" ")
dim(xx)

