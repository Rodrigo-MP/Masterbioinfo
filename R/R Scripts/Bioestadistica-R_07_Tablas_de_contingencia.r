################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
## 
## Sesión 07: Tablas de contingencia. Inferencia básica. Variables Categóricas.
##
################################################################################
################################################################################

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
## Descripción de una tabla de contingencia
################################################################################

t1 <- table(xx$raza, xx$bajo_pes )
t1

margin.table (t1, 1) ## Frecuencias marginales por filas 
margin.table (t1, 2) ## Frecuencias marginales por columnas

prop.table(t1)       ## Proporciones totales  
prop.table(t1, 1)    ## Proporciones por filas
prop.table(t1, 2)    ## Proporciones por columnas
round( 100*prop.table(t1, 1), dig=2 )    ## Porcentajes por filas

## Gráfico de barras para describir una tabla
dev.new()
par(mfrow=c(2,2))
barplot( t1 )
barplot( t(t1) )
barplot( t(t1), beside=T )
barplot( prop.table ( t(t1),2 ), beside=T )

## Gráfico de sectores para describir una tabla
dev.new()
par(mfrow=c(2,2))
lab.bajo=c("Bajo peso", "Peso normal")
col.1 = c("red", "blue")
pie( t1[1,], col=col.1, main="Raza Blanca", lab=lab.bajo )
pie( t1[2,], col=col.1, main="Raza Negra", lab=lab.bajo )
pie( t1[3,], col=col.1, main="Otras Razas", lab=lab.bajo )


################################################################################
## Tests de independencia: Chi2 y Fisher
################################################################################

## Test de chi-cuadrado
chisq.test(t1)
chisq.test(t1)$p.value
chisq.test(t1)$expected
## Test exacto de Fisher
fisher.test(t1)
fisher.test(t1)$p.value



################################################################################
## Informe de análisis de tablas de contingencia
################################################################################
 
## Se cargan las funciones  (se ejecuta el script)
source ( "C://Bioestadistica con R/R Scripts/Bioestadística-R Funciones.r" ) 
 
## Se abre el fichero de salida y se escribe el título y la cabecera
FileOut <-file("C://Bioestadistica con R/Temp/Bajo peso Tablas Contingencia.csv","w")

cat ( "Tabla: Tablas de Contingencia. Variables Categóricas", file=FileOut, sep="\n")
cat ( "", file=FileOut, sep="\n")
cat ( ";;Peso normal;Bajo peso;P-value", file=FileOut, sep="\n")
cat ( ";;N(%);N(%)", file=FileOut, sep="\n")

## Tabla de frecuencias de la variable binaria Y
t0 <- table (xx$bajo_pes)
p0 <- round ( 100 * prop.table ( t0 ), dig = 2)
cat ( paste ( "Total;;", t0[1], " (", p0[1], ");", t0[2], " (", p0[2], ");" , sep=""), 
      file=FileOut, sep="\n")

## Tablas de contingencia de las variables independientes Xs
AnalisisBinariaCategorica ( xx$raza, xx$bajo_pes , FileOut, "Raza" )
AnalisisBinariaCategorica ( xx$fumador, xx$bajo_pes , FileOut, "Fumador" )
AnalisisBinariaCategorica ( xx$part_pre, xx$bajo_pes , FileOut, "Partos prematuros" )
AnalisisBinariaCategorica ( xx$hta, xx$bajo_pes , FileOut, "HTA" )
AnalisisBinariaCategorica ( xx$irr_urin, xx$bajo_pes , FileOut, "Irritab. urinaria" )

close(FileOut)


################################################################################
## Riesgo Relativo (RR) y Odds Ratio (OR)
################################################################################

t1 <- table (  xx$fumador, xx$bajo_pes)
t1

a <- t1[2,2]   ## E=1  F=1
b <- t1[2,1]   ## E=0  F=1
c <- t1[1,2]   ## E=1  F=0
d <- t1[1,1]   ## E=0  F=0

## Riesgo Relativo (RR)                      
RR <- (a/(a+b)) / (c/(c+d))
RR

## Odds Ratio (OR)
OR <- (a*d) / (b*c)
OR



