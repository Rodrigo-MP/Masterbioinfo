################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
## 
## FUNCIONES que se han generado en todas las sesiones del curso
##                
################################################################################
################################################################################
                                                  

################################################################################
## Función que calcula las medidas resumen de una variable continua 
################################################################################

AnalisisContinua <- function ( var )
{
  if ( is.factor(var) == TRUE )
  { stop ( "La variable no es continua" ) }
                                                  
  ## Medidas de tendencia central
  mean = mean (var, na.rm=TRUE)
  median = median(var, na.rm=TRUE)
  mean.trim.10 = mean(var, trim=0.10)
  
  ## Medidas de Dispersión - Variabilidad
  sd = sd (var, na.rm=TRUE)
  iqr = IQR (var, na.rm=TRUE)   
  mad = mad (var, na.rm=TRUE) 

  list ( mean=mean, median=median, mean.trim.10 = mean.trim.10, 
         sd=sd, iqr=iqr, mad=mad)
}



################################################################################
## Función que calcula las medidas resumen de una variable categórica 
################################################################################

AnalisisCategorica <- function ( var )
{  
  ## Tabla, proporciones y porcentajes
  t1 = table ( var )
  p1 = prop.table( t1 )
  porc = round ( 100 * p1, dig = 2 ) 

  list ( tabla = t1, prop = p1, porc = porc  )
}



################################################################################
## Función para analizar la relación entre variables categóricas y una variable
## dependiente binaria (sesión 7)
################################################################################

AnalisisBinariaCategorica <- function ( x, y, File, titulo )
{
## Tabla de frecuencia y porcentajes por filas
  t1 <- table ( x, y )
  p1 <- round ( 100 * prop.table ( t1, 1 ), dig = 2) 
  
## Titulo y P-valor del test chi-cuadrado  
  cat( paste( titulo, ";;;;" , round( chisq.test(t1)$p.value, dig = 4 ) , sep=""),
        file=File, sep="\n" )

## Una línea por cada categoría de X  (filas de la tabla)
  for (i in 1:nrow(t1))
  {
    cat ( paste ( ";", names(t1[,1])[i], ";", 
                  t1[i,1], " (", p1[i,1], ");", t1[i,2], " (", p1[i,2], ");" , sep=""), 
         file=File, sep="\n")
  }
}



################################################################################
## Función para analizar la relación entre variables continuas y una variable
## dependiente binaria (sesión 8)
################################################################################

AnalisisBinariaContinua <- function ( x, y, File, titulo )
{
## Categorías de la variable Y 
  cat.y <- names( table ( y ) )

## Titulo y P-valor de los tests de igualdad de varianza, T de Student con 
## varianzas distintas, test de Mann-Whitney  

  cat( paste( titulo, ";;;;;" , 
              round( var.test ( x ~  y)$p.value , dig = 4 ) , ";", 
              round( t.test ( x ~  y)$p.value , dig = 4 ) , ";",
              round( wilcox.test ( x ~  y)$p.value , dig = 4 ) , sep=""),
        file=File, sep="\n" )

## Una línea por cada categoría de X, que incluye media(SD), mediana(MAD) y test
## de Kolmogorov de Normalidad
  for (i in 1:length(cat.y))
  {
    cat ( paste ( ";", cat.y[i], ";", 
                  round( mean( x[y==cat.y[i]] ), dig=2)  , " (", 
                  round( sd( x[y==cat.y[i]] ), dig=2)    , ");", 
                  round( median( x[y==cat.y[i]] ),dig=2) , " (", 
                  round( mad( x[y==cat.y[i]] ), dig=2)   , ");" ,
                  round( lillie.test ( x[y==cat.y[i]])$p.value , dig=4 )  , 
                  sep=""), 
         file=File, sep="\n")
  }
}



################################################################################
## Cálculo de la densidad bivariante con una estimación (sesión 10)
## Autor: Everitt
################################################################################


bivden<-function(x, y, ngridx = 30, ngridy = 30, constant.x = 1, constant.y = 1) {
	#x and y are vectors containing the bivariate data
	#ngridx and ngridy are the number of points in the grid
	#
	mx <- mean(x)
	sdx <- sqrt(var(x))
	my <- mean(y)
	sdy <- sqrt(var(y))
	#scale x and y before estimation
	x <- scale(x)
	y <- scale(y)
	#
	den <- matrix(0, ngridx, ngridy)
	#
	#find possible value for bandwidth
	#
	n <- length(x)
	#
	hx <- constant.x * n^(-0.2)
	hy <- constant.y * n^(-0.2)
	h <- hx * hy
	hsqrt <- sqrt(h)
	#
	seqx <- seq(range(x)[1], range(x)[2], length = ngridx)
	seqy <- seq(range(y)[1], range(y)[2], length = ngridy)
	#
	for(i in 1:n) {
		X <- x[i]
		Y <- y[i]
		xx <- (seqx - X)/hsqrt
		yy <- (seqy - Y)/hsqrt
		den <- den + outer(xx, yy, function(x, y)
			exp(-0.5 * (x^2 + y^2)))
			}
		den <- den/(n * 2 * pi * h)
		seqx <- sdx * seqx + mx
	seqy <- sdy * seqy + my
	result <- list(seqx = seqx, seqy = seqy, den = den)
	result
}



################################################################################
## Ajuste del Modelo de Regresión Logística: R2
################################################################################

LogisticModelFit <- function ( mod )  
{
  ## tamaño muestral 
  n <- mod$df.null+1
  
  LLB <- - ( mod$deviance  /2 )
  LL0 <- - ( mod$null.deviance /2 )
  L0 <- exp (LL0)
  LB <- exp (LLB)

  R2Cox <- 1 - (L0/LB)**(2/n)
  R2max <- 1 - L0**(2/n)
  R2Nag <- R2Cox /R2max

  list ( R2Cox=R2Cox, R2Nag=R2Nag )

}




################################################################################
## Regresión Logística: Análisis de una variable continua con percentiles
##
##    dep.var     = vector con la variable binaria dependiente
##    cont.var    = vector con la variable independiente continua
##    titulo      = texto para incluir en los títulos, nombre de la variable
##    percentiles = contiene el número de grupos en los que se dese partir la
##                  variable. Por ejemplo: 3 serán terciles, 4 cuartiles, .... 
################################################################################

Analisis_Percentiles_Logistica <- function( dep.var, cont.var, titulo="",  
                                            percentiles = 3:5 ) 
{

  ## Panel con 4 gráficos, 1º gráfico histograma 
  dev.new()  
  par(mfrow=c(2,2))
  hist(cont.var, breaks=25, col="red" , main=titulo)


  ## Tratamiento por percentil
  
  for ( num_quant in percentiles )              
  {

    ## Calculamos los percentiles
    pto <- as.numeric ( quantile( cont.var, (0:num_quant)/num_quant, na.rm=T) )
  
    ## Añadimos 1 al máximo porque si no se queda fuera   
    pto [length (pto) ] <- pto [length (pto) ] + 1 
  
    ## Se define la variable categórica   
    cat.var <- cut( cont.var , pto , labels=1:(length(pto)-1) , right=F )
    
    ## Regresión Logistica con la variable categórica
    log.out <- glm ( dep.var ~ cat.var, family = binomial  )

    ###########################   
    ## Reportamos resultados

    print ( "--------------------------------------------------" )
    print ( "Puntos de corte son:" )
    print ( pto[2:(length(pto)-1)] )
    print ( "Tabla" )
    print ( table (cat.var) ) 

    for ( j in 2:length(log.out$coef))
    {
      print( paste( names(log.out$coef)[j], " Beta = ", 
             round ( summary(log.out)$coef [j,1] , d=2), " ( ", 
             round ( summary(log.out)$coef [j,1] - 1.96 * summary(log.out)$coef [j,2], d=2), " - ",
             round ( summary(log.out)$coef [j,1] + 1.96 * summary(log.out)$coef [j,2], d=2),  " )   P=",
             round ( summary(log.out)$coef [j,4] , d=4),          
                sep=""  ) )    
    }

    ###########################   
    ## Gráfico de dispersión de los Beta frente a los percentiles 

    ## Puntos medios de los puntos de corte
    midpoints <- (  pto[ 1:(length(pto)-1) ] + pto[ 2:length(pto) ] ) / 2

    ## Coeficientes B, añadiendo el 0 para la categoría de referecncia
    coeff <- c( 0, log.out$coef[2:length(log.out$coef)] ) 

    ## Plot  
    plot( midpoints, coeff,  
          main=paste(titulo, length(midpoints), "grupos"), type="b") 

    abline(h=0)

  } #end_for percentiles

} #end_function

