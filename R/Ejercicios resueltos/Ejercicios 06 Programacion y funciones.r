################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##
## Ejercicios Sesión 06: Programación y funciones
##       
################################################################################
################################################################################

################################################################################
## Fichero Datos: Bajo peso al nacer
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/Bajo peso al nacer.csv", sep=";")
dim(xx)
head(xx)

################################################
## Función que calcula las medidas resumen de una variable categórica 

AnalisisCategorica <- function ( var )
{  
  ## Tabla, proporciones y porcentajes
  t1 = table ( var )
  p1 = prop.table( t1 )
  porc = round ( 100 * p1, dig = 2 ) 

  list ( tabla = t1, prop = p1, porc = porc  )
}


################################################
## Ejecución de la función
AnalisisCategorica (xx$raza)
AnalisisCategorica (xx$fumador)
AnalisisCategorica (xx$hta)



################################################################################
## Análisis desciptivo de las variables categóricas
################################################################################

## Se abre el fichero de salida y se escribe el título y la cabecera
FileOut = file("C://Bioestadistica con R/Temp/Bajo Peso Variables categóricas.csv", "w")

cat ( "Tabla 1: Análisis descriptivo. Variables Categóricas", file=FileOut, sep="\n")
cat ( "", file=FileOut, sep="\n")
cat ( ";Grupo;N(%)", file=FileOut, sep="\n")

## Para cada variable categórica
for ( nombre.var in c ("bajo_pes", "raza", "fumador", "part_pre",
                       "hta", "irr_urin", "visi_med" ))
{
   ## Nombre de la variable
   cat ( nombre.var , file=FileOut, sep="\n")
   
   ## Llamada a la función
   out <- AnalisisCategorica ( xx[, nombre.var] )
   
   ## Escribe el nombre de la categoría, la frecuencia y el porcentaje
   for ( i in 1:length(out$tabla))
   {
     cat ( paste ( ";", names(out$tabla[i]) , ";" , out$tabla[i] , 
                   " (" , out$porc[i] , ")" , sep="") 
           , file=FileOut, sep="\n")     
   }
}

close(FileOut)
         
            
             