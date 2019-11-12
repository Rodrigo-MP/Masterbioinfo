################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##
## Ejercicios Sesión 03: Tratamiento de datos    
##
################################################################################
################################################################################

## Se leen el fichero 
f1 <- read.csv(file="C://Bioestadistica con R/Ficheros para importar/Ejemplo 7.csv",
                 header=T, sep=";" )
dim(f1)
f1

## Se leen el fichero 
f2 <- read.csv(file="C://Bioestadistica con R/Ficheros para importar/Ejemplo 8.csv",
                 header=T, sep=";" )
dim(f2)
f2

## Definimos fumador como factor
f1$fumador <- as.factor(f1$fumador)

## Edad categorizada
f1$edad60 <- as.integer ( f1$edad >= 60 )
f1
                   
## Se unen con merge
f3 <- merge ( f1, f2[ , c("id","med_2") ], by="id", all.x = TRUE)
 
 
## Se escribe el fichero
write.table ( f3, "C://Bioestadistica con R/Temp/Ejemplo 7_8.txt", 
            quote=FALSE , sep="\t", col.names=TRUE, row.names=FALSE)



