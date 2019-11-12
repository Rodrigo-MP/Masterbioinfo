################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##      
## Ejercicios Sesión 07: Análisis de Tablas de Contingencia
##
## Fichero de datos: umaru
##
################################################################################
################################################################################

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


###############################################################################
## Descripción de una tabla de contingencia
################################################################################

## Frecuencias absolutas
t1 <- table(xx$RACE, xx$DFREE )
t1

## Porcentajes
round( 100*prop.table(t1, 1), dig=2 )    ## Porcentajes por filas

## Gráfico de barras
dev.new()
par(mfrow=c(2,2))
barplot(t(t1))
barplot(prop.table ( t(t1),2 ), beside=T)

## Gráfico de sectores

col.1 = c("red", "blue")
pie(t1[1,], col=col.1, main="Raza Blanca")
pie(t1[2,], col=col.1, main="Otras Razas")


################################################################################
## Tests de independencia: Chi2 y Fisher
################################################################################

chisq.test( table(xx$IVHX, xx$DFREE ) )
chisq.test( table(xx$RACE, xx$DFREE ) )
chisq.test( table(xx$TREAT, xx$DFREE ) )
chisq.test( table(xx$SITE, xx$DFREE ) )


################################################################################
## Informe con las tablas de contingencia
################################################################################

## Se cargan las funciones
source ( "C://Bioestadistica con R/R Scripts/Estadística-R Funciones.r")

## Se abre el fichero de salida y se escribe el título y la cabecera
FileOut <- file("C://Bioestadistica con R/Temp/Umaru Tablas Contingencia.csv", "w")

cat ( "Tabla: Tablas de Contingencia. Variables Categóricas", file=FileOut, sep="\n")
cat ( "", file=FileOut, sep="\n")
cat ( ";;Recaida Drogas;Libre Drogas;P-value", file=FileOut, sep="\n")
cat ( ";;N(%);N(%)", file=FileOut, sep="\n")

## Tabla de frecuencias de la variable binaria Y
t0 <- table (xx$DFREE)
p0 <- round ( 100 * prop.table ( t0 ), dig = 2)
cat ( paste ( "Total;;", t0[1], " (", p0[1], ");", t0[2], " (", p0[2], ");" , sep=""), 
      file=FileOut, sep="\n")

## Tablas de contingencia de las variables independientes Xs
AnalisisBinariaCategorica ( xx$IVHX, xx$DFREE , FileOut, "Historia Consumo" )
AnalisisBinariaCategorica ( xx$RACE, xx$DFREE , FileOut, "Raza" )
AnalisisBinariaCategorica ( xx$TREAT, xx$DFREE , FileOut, "Tratamiento" )
AnalisisBinariaCategorica ( xx$SITE, xx$DFREE , FileOut, "Sitio" )

close(FileOut)



