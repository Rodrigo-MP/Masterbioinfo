################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##
## Ejercicios Sesión 05: Estadística Descriptiva
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
## Descripción de las variables categóricas
################################################################################

################################## 
## Tablas: frecuencias absolutas y proporciones

table(xx$IVHX)
round( 100 * prop.table(table(xx$IVHX)), dig=2 )

table(xx$RACE)
round( 100 * prop.table(table(xx$RACE)), dig=2 )

table(xx$TREAT)
round( 100 * prop.table(table(xx$TREAT)), dig=2 )

table(xx$SITE)
round( 100 * prop.table(table(xx$SITE)), dig=2 )

table(xx$DFREE)
round( 100 * prop.table(table(xx$DFREE)), dig=2 )


################################## 
## Gráficos de barra
 
dev.new() 
par(mfrow=c(2,3))  

barplot(table(xx$IVHX), col="red", main="IVHX")    
barplot(table(xx$RACE), col="red", main="RACE")      
barplot(table(xx$TREAT), col="red", main="TREAT")    
barplot(table(xx$SITE), col="red", main="SITE")    
barplot(table(xx$DFREE), col="red", main="DFREE")    




################################################################################
## Descripción de las variables cuantitativas
################################################################################

################################## 
## Medias y SD

mean(xx$AGE)
sd(xx$AGE)

mean(xx$BECK)
sd(xx$BECK)

mean(xx$NDRUGTX)
sd(xx$NDRUGTX)


################################## 
## Gráficos: histograma y boxplot

dev.new()
par(mfrow=c(2,3)) 

hist(xx$AGE, col="red", main="Histograma de AGE", xlab="AGE", ylab="Frecuencia")
hist(xx$BECK, col="red", main="Histograma de BECK", xlab="BECK", ylab="Frecuencia")
hist(xx$NDRUGTX, col="red", main="Histograma de NDRUGTX", xlab="NDRUGTX", ylab="Frecuencia")
              
boxplot(xx$AGE, col="red", main="Boxplot de AGE", xlab="AGE")
boxplot(xx$BECK, col="red", main="Boxplot de BECK", xlab="BECK")
boxplot(xx$NDRUGTX, col="red", main="Boxplot de NDRUGTX", xlab="NDRUGTX")


################################## 
## Normalidad

shapiro.test(xx$AGE)
lillie.test(xx$AGE)

shapiro.test(xx$BECK)
lillie.test(xx$BECK)

shapiro.test(xx$NDRUGTX)
lillie.test(xx$NDRUGTX)




                       