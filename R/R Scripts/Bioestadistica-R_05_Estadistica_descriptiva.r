################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica 
##
## Autor: Jes�s Herranz
## 
## Sesi�n 05: Estad�stica Descriptiva
##
################################################################################
################################################################################

library(nortest)

################################################################################
## Fichero Datos: Bajo peso al nacer
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/Bajo peso al nacer.csv", sep=";")
dim(xx)
head(xx)

## Variables categ�ricas

xx$bajo_pes <- factor(xx$bajo_pes)
xx$raza     <- factor(xx$raza)
xx$fumador  <- factor(xx$fumador)
xx$part_pre <- factor(xx$part_pre)
xx$hta      <- factor(xx$hta)
xx$irr_urin <- factor(xx$irr_urin)


################################################################################
## Estad�stica Descriptiva. Variables categ�ricas
################################################################################

## Frecuencias absolutas
t1 <- table(xx$bajo_pes)
t1
## Frecuencias relativas. Proporciones
p1 <- prop.table(t1)
p1
## Porcentajes
p1 * 100
round( p1 * 100, dig=2)

## Gr�fico de sectores
dev.new()
pie(t1)
pie(t1, col=c("red","blue"), main="Bajo peso al nacer", 
        labels=paste(names(t1), " (", round( p1 * 100, dig=2),"%)", sep="") )

## Gr�fico de barras
dev.new()
barplot(t1)
barplot(t1, col=c("red","blue"), main="Bajo peso al nacer", 
        names.arg=paste(names(t1), " (", round( p1 * 100, dig=2),"%)", sep="") )


################################################################################
## Estad�stica Descriptiva. Variables continuas
################################################################################

## Medidas de tendencia central
mean(xx$edad)
median(xx$edad)
mean(xx$edad, trim=0.10)

## Cuartiles y percentiles
quantile(xx$edad, c(0.10,0.25,0.50,0.75,0.90))

## Medidas de dispersi�n
var(xx$edad)
sum( (xx$edad - mean(xx$edad))^2 ) / (length(xx$edad)-1)
sd(xx$edad)
100 * abs(sd(xx$edad) / mean(xx$edad))
IQR(xx$edad)
mad(xx$edad)

## Histograma
dev.new()
hist(xx$edad)
hist(xx$edad, col="red", main="Histograma de Edad", 
              xlab="Edad", ylab="Frecuencia",
              xlim=c(10,50), breaks=seq(10,50, by=2))

## Boxplot
dev.new()
boxplot(xx$edad)
boxplot(xx$edad, col="red", main="Boxplot de Edad", xlab="Edad")

## Histograma y boxplot de la variable peso
dev.new()
par(mfrow=c(1,2))
hist(xx$peso, col="red", main="Histograma de Peso de la madre", 
              xlab="Peso", ylab="Frecuencia",
              xlim=c(30,120), breaks=seq(30,120, by=5))
boxplot(xx$peso, col="red", main="Boxplot de Peso de la madre", xlab="Peso")


################################################################################
## Estad�stica Descriptiva
################################################################################

summary(xx)


################################################################################
## Normalidad
################################################################################

## Test de hipotesis
shapiro.test(xx$edad)
lillie.test(xx$edad)

shapiro.test(xx$peso)
lillie.test(xx$peso) 

## QQ Plot
dev.new()
par(mfrow=c(1,2))
qqnorm(xx$edad, main="Normal Q-Q Plot - Edad")
qqline(xx$edad)
qqnorm(xx$peso, main="Normal Q-Q Plot - Peso")
qqline(xx$peso)

################################################################################
## Funciones de probabilidad
################################################################################

## Funci�n de densidad de una Normal est�ndar
dnorm(0, mean=0, sd=1)

## Funci�n de distribuci�n de una Normal est�ndar
pnorm(1.96, mean=0, sd=1)
pnorm(0, mean=0, sd=1)
pnorm(-1.96, mean=0, sd=1)

## Funci�n de cuantiles de una Normal est�ndar
qnorm(0.025, mean=0, sd=1)
qnorm(0.975, mean=0, sd=1)
qnorm(0.025, mean=0, sd=1, lower.tail=FALSE)

## Generar Normal est�ndar
norm.st <- rnorm(1000, mean=0, sd=1)
mean(norm.st)                                   
sd(norm.st)

## Gr�fico de una Normal est�ndar
x<-seq(-4,4,by=0.1)
dev.new()
plot(x,dnorm(x), type="l")

              