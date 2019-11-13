################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica
##
## Autor: Jes�s Herranz
##
## Ejercicios Sesi�n 06: Programaci�n y funciones
##
################################################################################
################################################################################

################################################################################
## Fichero Datos: Bajo peso al nacer
################################################################################

xx <- read.csv(file="/home/rodrigo/github/Masterbioinfo/R/Datos/Bajo peso al nacer.csv", sep=";")
dim(xx)
head(xx)

################################################
## Funci�n que calcula las medidas resumen de una variable categ�rica

AnalisisCategorica <- function ( var )
{
  ## Tabla, proporciones y porcentajes
  t1 = table ( var )
  p1 = prop.table( t1 )
  porc = round ( 100 * p1, dig = 2 )

  list ( tabla = t1, prop = p1, porc = porc  )
}

ls()
################################################
## Ejecuci�n de la funci�n
AnalisisCategorica (xx$raza)
AnalisisCategorica (xx$fumador)
AnalisisCategorica (xx$hta)



################################################################################
## An�lisis desciptivo de las variables categ�ricas
################################################################################

## Se abre el fichero de salida y se escribe el t�tulo y la cabecera
FileOut = file("C://Bioestadistica con R/Temp/Bajo Peso Variables categ�ricas.csv", "w")

cat ( "Tabla 1: An�lisis descriptivo. Variables Categ�ricas", file=FileOut, sep="\n")
cat ( "", file=FileOut, sep="\n")
cat ( ";Grupo;N(%)", file=FileOut, sep="\n")

## Para cada variable categ�rica
for ( nombre.var in c ("bajo_pes", "raza", "fumador", "part_pre",
                       "hta", "irr_urin", "visi_med" ))
{
   ## Nombre de la variable
   cat ( nombre.var , file=FileOut, sep="\n")

   ## Llamada a la funci�n
   out <- AnalisisCategorica ( xx[, nombre.var] )

   ## Escribe el nombre de la categor�a, la frecuencia y el porcentaje
   for ( i in 1:length(out$tabla))
   {
     cat ( paste ( ";", names(out$tabla[i]) , ";" , out$tabla[i] ,
                   " (" , out$porc[i] , ")" , sep="")
           , file=FileOut, sep="\n")
   }
}

close(FileOut)
