################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica
##
## Autor: Jes�s Herranz
##
## Ejercicios Sesi�n 03: Tratamiento de datos
##
################################################################################
################################################################################

## Se leen el fichero
f1 <- read.csv(file="/home/rodrigo/github/Masterbioinfo/R/Ficheros para importar/Ejemplo 7.csv",
                 header=T, sep=";" )
dim(f1)
f1

## Se leen el fichero
f2 <- read.csv(file="/home/rodrigo/github/Masterbioinfo/R/Ficheros para importar/Ejemplo 8.csv",
                 header=T, sep=";" )
dim(f2)
f2

## Definimos fumador como factor
f1$fumador <- as.factor(f1$fumador)

## Edad categorizada
f1$edad60 <- as.integer ( f1$edad >= 60 )
#le asigna la categoría edad60 a todos los datos que pertenezcan al grupo de lo de dentro del paréntesis
f1

## Se unen con merge
f3 <- merge ( f1, f2[ , c("id","med_2") ], by="id", all.x = TRUE)


## Se escribe el fichero
write.table ( f3, "/home/rodrigo/github/Masterbioinfo/R/Ejemplo 7_8.txt",
            quote=FALSE , sep="\t", col.names=TRUE, row.names=FALSE)
