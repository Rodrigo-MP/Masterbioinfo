################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##       
## COMPLEMENTARIO: Gráficos de las presentaciones del Material Docente
##
################################################################################
################################################################################
                                                     

################################################################################
## Distribución de probabilidad
################################################################################

## Gráfico de la distribución t de Student con 1 df
dev.new()                                                
x<-seq(-10,10,by=0.01)
plot(x,dt(x, df=1), ylim=c(-0.01, 0.35),  
     ylab="", main="t de Student", xlab="", type="l", lwd=3)

segments ( -4, 0, -4, dt(-4, df=1) , lwd=3 ) 
for ( i in c (-4.5, -5, -5.5, -6, -6.5, -7, -7.5, -8 ))
{ segments ( i, 0, i+0.5, dt(i+0.5, df=1) , lwd=0.5 )  }

abline(h=0)
text(-4, 0.03, "f(x)", cex=1.5)
text(-6, 0.07, "p=F(q)", cex=1.5)
text(-4, -0.01, "q,x", cex=1.5)


################################################################################
## Contraste de hipótesis basado en una chi2-cuadrado
################################################################################

## Valor téorico con alfa=0.05 de un chi2 con 4 df
alfa = 0.05

chi2.teorica  <- qchisq(1-alfa, df=4 )

## Valor muestral: se supone que es el valor obtenido en la muestra
chi2.muestral <- 11.5
p <- 1 - pchisq(11.5, df=4)


## Prepara el gráfico de la distribución chi2, pero no se grafica todavía
dev.new()
x<-seq(0,16,by=0.1)
plot(x,dchisq(x, df=4), type="n", xaxp = c(0,16,8),  
     ylab="Densidad de chi2-cuadrado", xlab="")

## Area de 0.05 de la cola, por debajo de la curva
## Se hace con muchos polígonos
x.teor <- seq (chi2.teorica ,16,by=0.1)
y.teor <- dchisq(x.teor, df=4)

for ( i in 1:length(x.teor) )
{
  polygon( c ( x.teor [i], x.teor [i], x.teor [i+1], x.teor [i+1]  ) ,
           c (          0, y.teor [i], y.teor [i+1], 0 ) ,
           col="gray", border="gray")
}

## Gráfico de la función de distribución chi2
lines(x,dchisq(x, df=4),  lwd=3)
abline(h=0)

## Situación de ch2.teorica y chi2.muestral (es decir alfa y P-valor)
segments ( chi2.teorica, 0, chi2.teorica, dchisq (chi2.teorica, df=4),
           col="blue", lwd=3)
segments ( chi2.muestral, 0, chi2.muestral, dchisq (chi2.muestral, df=4),
           col="red", lwd=3)
           
           
################################################################################
## Generación de una ejemplo para ANOVA
################################################################################


x <- rnorm( 50, 50, 10)

gr <- c( rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10) )


summary( aov ( x ~ gr ))

tapply ( x, gr, mean )
tapply ( x, gr, sd )

dev.new()
##stripchart ( x ~ gr,  method="jitter" )
stripchart ( x ~ gr,  , pch ="|" , ylim=c(0,5)) ##, yaxs=c("medias",1,2,3,4,5) )
abline ( h=1)
abline ( h=2)
abline ( h=3)
abline ( h=4)
abline ( h=5)
points ( tapply ( x, gr, mean ), c(1,2,3,4,5), cex=1.2, pch="x")

## última línea para las medias solo
abline ( h=0)
points ( tapply ( x, gr, mean ), c(0,0,0,0,0), cex=1.2, pch="x")


################################################################################
## Análisis de correlaciones
## Gráficos de correlaciones lineales y no lineales
################################################################################

num = 30

X11(20,8)

par( mfrow = c(1,3))

## linel
x2 = runif ( num , 0, 5 )
y2 =   1.2 * x2 + 7 + rnorm ( num, 0, 1 )
plot( x2, y2, pch=16, xlab="", ylab="", cex=1.4 )
round ( cor ( x2,y2, method = "pearson" ), 2 )
round ( cor ( x2,y2, method = "spearman" ), 2 )
abline( lm(y2 ~ x2 ) )

## Exponecial
x1 = runif ( num , 0, 5 )
y1 = exp ( x1 ) + rnorm ( num, 0, 3 )
plot( x1, y1 , pch=16, xlab="", ylab="", cex=1.4 )
round ( cor ( x1,y1, method = "pearson" ), 2 )
round ( cor ( x1,y1, method = "spearman" ), 2 )
abline( lm(y1 ~ x1 ) )

## Cuadrática
x3 = runif ( num , -5, 5 )
y3 =  2*x3 ** 2  + rnorm ( num, 0, 1.5 )
plot( x3,y3, pch=16, xlab="", ylab="", cex=1.4)
round ( cor ( x3,y3, method = "pearson" ), 2 )
round ( cor ( x3,y3, method = "spearman" ), 2 )
abline( lm(y3 ~ x3 ) )


################################################################################
## Cuadro
X11()
plot ( c(0),c(0), xlab="", ylab="", type="n" , xaxp=c(-10,10,1), yaxp=c(-10,10,1) )
abline (h=0)
abline (v=0)


################################################################################
## Regresión lineal simple
## Gráfico 1: E(y) en función de X
################################################################################

x <- c(10,10,10,20,20,20,20,20,30,30,30,30,30,40,40,40,40)
y <- c(12,17,22,20,16,31,27,28,24,38,43,36,35,43,38,49,44)

fit <- lm(y~x)


dev.new()
plot(x,y, type="n", xlim=c(5,45), ylim=c(5,55),  xaxp=c(0,1,1), yaxp=c(0,1,1))
abline(fit, lwd=2)
axis(1, at=c(10,20,40), lab=c("x1","x2","x3"))

y1<-fitted(fit)[1]
y2<-fitted(fit)[5]
y3<-fitted(fit)[15]

axis(2, at=c(y1,y2,y3), lab=c("E(y1)","E(y2)","E(y3)"))

abline(v=10, lty=3, lwd=2)
abline(v=20, lty=3, lwd=2)
abline(v=40, lty=3, lwd=2)
segments(1,y1, 10, y1, lwd=2)
segments(1,y2, 20, y2, lwd=2)
segments(1,y3, 40, y3, lwd=2)


################################################################################
## Regresión lineal simple
## Gráfico 2: beta + y beta -
################################################################################

## Beta +
dev.new()
plot(x,y, type="n", xlim=c(5,45), ylim=c(5,55),  xaxp=c(0,1,1), yaxp=c(0,1,1))
axis(1, at=c(20,25), lab=c("x","x+1"), cex.axis=1.5)
abline(b=1,a=10, lwd=2)
abline(v=20, lty=3, lwd=2)
abline(v=25, lty=3, lwd=2)
segments(20, 20+10, 25, 20+10, lwd=2)
segments(25, 20+10, 25, 25+10, lwd=2)

## Beta -
dev.new()
plot(x,y, type="n", xlim=c(5,45), ylim=c(5,55),  xaxp=c(0,1,1), yaxp=c(0,1,1))
axis(1, at=c(20,25), lab=c("x","x+1"), cex.axis=1.5)
abline(b=-1,a=50, lwd=2)
abline(v=20, lty=3, lwd=2)
abline(v=25, lty=3, lwd=2)
segments(20, 20+10, 25, 20+10, lwd=2)
segments(25, 20+10, 25, 25, lwd=2)



################################################################################
## Regresión lineal simple
## Gráfico 3: variabilidad (residuos, valores ajustados)
################################################################################

## Beta +
dev.new()
plot(x,y, type="n", xlim=c(10,45), ylim=c(15,50),  xaxp=c(0,1,1), yaxp=c(0,1,1))
#axis(1, at=c(20), lab=c("x"), cex.axis=1.5)
points(30,40, cex=1.5,pch=16)
abline(b=1,a=5, lwd=2)
segments ( 1, 40, 30, 40, lwd=1.5)
segments ( 1, 35, 30, 35, lwd=1.5)
segments ( 1, 32, 30, 32, lwd=1.5)
#segments ( 30, 1, 30, 40, lwd=1.5)
segments ( 30, 32, 30, 35, lwd=2.5, col="blue")
segments ( 30, 35, 30, 40, lwd=2.5, col="red")
segments ( 40, 32, 40, 40, lwd=2.5, col="green")


################################################################################
## Regresión lineal múltiple
## 3 modelos de gráficos de Residuos frente a ajustados
################################################################################

ajust      <- c(71,82,90,86,98,107,91,90,90,109,101,93,145,138,
            110,112,118,103,132,101,112,102,122,128,169,140,170,152,164,138) 

## aletoriamente repartidos (OK)
res.ok     <- c(24,3,10,-1,-3,-27,-26,20,-20,-14,9,-3,-10,-32,6,-6,
                16,31,33,19,18,-17,-37,32,-4,-45,25,7,-6,-1)
            
## No linealidad
ajust.noline <- c(71,82,90,86,98,107,91,90,90,79,101,93,
                  90,112,118,103,132,101,112,102,122,128,169,140,170,150,145,158,154) 
                  
res.noline <-   c(-24,3,10,-1,-3,-7,-26,20,-20,-14,9,-3,-10,22,
                  16,31,23,9,18,-27,27,28,-4,25,- 25, 15, 18,-10,-7)             

## Heterocedasticidad (varianza aumenta)
ajust.hetero  <- c(71,82,90,86,98,107,91,90,90,109,101,93,
                   110,112,118,103,132,101,112,102,122,128,169,140,170,152,164,138,
                   140,170,145,156,165,137) 

res.hetero <- c(4,3,10,-1,-3,-17,-16,10,-20,-14,9,-3,-10,-32,
                16,11,33,19,18,-17,-37,32,-4,-45,25,-23,35,-39,
                0,49,-55,10,-22,-7)   

dev.new()
par(mfrow=c(1,3))
plot(ajust, res.ok, xlab="Ajustados", ylab="Residuos")
plot(ajust.noline, res.noline, xlab="Ajustados", ylab="Residuos")
plot(ajust.hetero, res.hetero, xlab="Ajustados", ylab="Residuos")


################################################################################
## Regresión Logística: Función Logística
################################################################################

X<-seq(-4,4,by=0.01)
length(X)

## Parámetro B negativo y positivo

fz_pos <- 1  / ( 1+exp(-(2)*X))
fz_neg <- 1  / ( 1+exp(-(-2)*X))

dev.new()
par(mfrow=c(1,2))

plot(X, fz_pos, type="l", col="red", lwd=3, cex.main=0.9, cex.axis = 0.8,
     main="Función Logística con B>0", ylab="Probabilidad" )
plot(X, fz_neg, type="l", col="red", lwd=3, cex.main=0.9, cex.axis = 0.8,
     main="Función Logística con B<0", ylab="Probabilidad" )


################################################################################
## Regresión Logística: Función de verosimilitud
################################################################################

x = seq (0,1,by=0.001)
menos.log = - log (x)

dev.new()
plot( x, menos.log, xlab="Likelihood", ylab="- LL ( - ln L )",  
      cex.lab=1.5, font.lab=2, font.axis=2, lwd=5, type="l", col="red" )


################################################################################
## Análisis de Supervivencia
## Grafico mostrando los tiempos de seguimiento y supervivencia
################################################################################

lwd1 <- 2  # size for line
lwd2 <- 4  # size for points


dev.new()
par(mfrow=c(1,2))


##################
## Graph nº 1

plot(c(-1),c(-1), xlim=c(0,50), ylim=c(0.9,8),   axes=FALSE, xlab="Tiempo", ylab="Observaciones")
axis(1, labels=seq(0,50, by=5) , at=seq(0,50,by=5))
axis(2, labels=8:1 , at=1:8)

segments(0,8,42,8,lwd=lwd1)
points(42,8,pch=1,lwd=lwd2)

segments(7,7,24,7,lwd=lwd1)
points(24,7,pch=4,lwd=lwd2)

segments(0,6,18,6,lwd=lwd1)
points(18,6,pch=4,lwd=lwd2)

segments(24,5,50,5,lwd=lwd1)
points(50,5,pch=1,lwd=lwd2)

segments(0,4,50,4,lwd=lwd1)
points(50,4,pch=1,lwd=lwd2)

segments(12,3,50,3,lwd=lwd1)
points(50,3,pch=1,lwd=lwd2)

segments(0,2,33,2,lwd=lwd1)
points(33,2,pch=1,lwd=lwd2)

segments(0,1,10,1,lwd=lwd1)
points(10,1,pch=4,lwd=lwd2)


##################
## Graph nº 2

plot(c(-1),c(-1), xlim=c(0,50), ylim=c(0.9,8),   axes=FALSE, xlab="Tiempo", ylab="Observaciones")
axis(1, labels=seq(0,50, by=5) , at=seq(0,50,by=5))
axis(2, labels=8:1 , at=1:8)

segments(0,8,42,8,lwd=lwd1)
points(42,8,pch=1,lwd=lwd2)

segments(0,7,24-7,7,lwd=lwd1)
points(24-7,7,pch=4,lwd=lwd2)

segments(0,6,18,6,lwd=lwd1)
points(18,6,pch=4,lwd=lwd2)

segments(0,5,50-24,5,lwd=lwd1)
points(50-24,5,pch=1,lwd=lwd2)

segments(0,4,50,4,lwd=lwd1)
points(50,4,pch=1,lwd=lwd2)

segments(0,3,50-12,3,lwd=lwd1)
points(50-12,3,pch=1,lwd=lwd2)

segments(0,2,33,2,lwd=lwd1)
points(33,2,pch=1,lwd=lwd2)

segments(0,1,10,1,lwd=lwd1)
points(10,1,pch=4,lwd=lwd2)


################################################################################
## Análisis de Componentes Principales
################################################################################

X1 <- c(15,21,26,26,28,29,25,29,30,32,26,28,29,25,29,30,32,33,33,33,34,40,42,36)
X2 <- c(32,37,42,47,45,58,56,52,53,59,49,49,55,56,50,57,58,64,60,70,74,78,80,79)
dev.new() 
par(mfrow=c(1,2))
plot(X1,X2, pch=16, cex=1.2)
text(38,35,paste("r=",round(cor(X1,X2),dig=3) ))
plot(X1,X2, xlim=range(X1,X2), ylim=range(X1,X2) , pch=16, cex=1.2)


# PCA

data.w <- cbind(X1,X2)

pc.w <- princomp(data.w)
pc.w
summary(pc.w)
load.w <- pc.w$loading

## Gráficode las CPs 
slope <- load.w[2,] / load.w[1, ]
mn <- apply ( data.w, 2, mean)
intcpt <- mn[2] - (slope*mn[1])

abline(intcpt[1], slope[1], lwd=2)
abline(intcpt[2], slope[2], lwd=2, lty=2)


################################################################################
## Análisis Cluster
## Diastancias entre grupos
################################################################################

X1 <- c(22,24,28,30,31,33,35,36,38,50,54,56,59,60,62)
X2 <- c(30,21,25,18,30,25,23,33,29,31,27,31,26,29,24)

dev.new()
par (mfrow = c(1,2)) 
## gráfico solo con puntos
plot(X1,X2, pch=16, cex=1.5, ylim=c(10,40))

## gráfico con puntos donde se añaden las distancias
plot(X1,X2, pch=16, cex=1.5, ylim=c(10,40))

## Todas las distancias
for ( i in 1:9)       ## 1º cluster
for ( j in 10:15)     ## 2º cluster
{
   segments(X1[i],X2[i],X1[j],X2[j], col="gray45", lwd=0.4)
}

## Las 3 distancias: min, max, media
segments(38,29,50,31, col="blue", lwd=3)
segments(22,30,62,24, col="red", lwd=3)
segments(30,26,57,28, col="green", lwd=3)

legend ( 45, 15, c("min","max","media"), col=c("blue","red","green"), lty = 1, lwd=2)


