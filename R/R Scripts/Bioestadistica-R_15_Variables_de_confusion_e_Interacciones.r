################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##            
## Sesión 15: Variables de confusión e Interacciones
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

xx$raza     <- factor(xx$raza)
xx$fumador  <- factor(xx$fumador)
xx$part_pre <- factor(xx$part_pre)
xx$hta      <- factor(xx$hta)
xx$irr_urin <- factor(xx$irr_urin)

## Recodificaciones

xx$part_pre2 <- NA
xx$part_pre2 [ xx$part_pre == 0] <- 0
xx$part_pre2 [ xx$part_pre == 1] <- 1
xx$part_pre2 [ xx$part_pre == 2] <- 1
xx$part_pre2 [ xx$part_pre == 3] <- 1


################################################################################
## Variables de confusion
################################################################################

out.2 <- glm(bajo_pes ~ fumador , data=xx, family = binomial )
summary(out.2)

out.4 <- glm(bajo_pes ~ fumador + raza , data=xx, family = binomial )
summary(out.4)

abs(1.116 - 0.704 ) / 0.704

## ORs
coef.4 <- summary(out.4)$coeff

for ( i in 2:4)
{ print ( paste ( row.names(coef.4)[i], "  " ,
                  round( exp( coef.4[i,1] ), dig=2) , 
                  " (", 
                  round( exp( coef.4[i,1] - 1.96 * coef.4[i,2] ), dig=2) , 
                  "-" ,
                  round( exp( coef.4[i,1] + 1.96 * coef.4[i,2] ), dig=2) ,
                  ")", sep ="") )
}

## Test del cociente de verosimilitudes
anova(out.4, test="Chisq")
 
## Relación entre la variable predictora y la variable de confusión
table(xx$raza, xx$fumador)
prop.table(table(xx$raza, xx$fumador), 1)
chisq.test(table(xx$raza, xx$fumador)) 
 
 
################################################################################
## Interacción
################################################################################ 

## Crear la variable peso dicotómica 
xx$peso_gr <- NA 
xx$peso_gr [ xx$peso < 49.5 ] <- 1
xx$peso_gr [ xx$peso >= 49.5 ] <- 0

## Modelo con interacción 
out.5.int <- glm(bajo_pes ~ peso_gr * edad , data=xx, family = binomial )
summary(out.5.int)

## Test del cociente de verosimilitudes para evaluar la interacción
out.5.main <- glm(bajo_pes ~ peso_gr + edad , data=xx, family = binomial )
summary(out.5.main)

anova(out.5.int, out.5.main, test="Chisq")


#####################################
## Interpretación de la interacción
## Intervalos de confianza para los ORs

## Coeficientes de Regresión
coef.5 <- summary(out.5.int)$coeff

B1<-coef.5[2,1]  # peso_gr
B2<-coef.5[3,1]  # edad
B3<-coef.5[4,1]  # Interacción

## Matriz de covarianzas entre los coeficientes
cov.est <- summary(out.5.int)$cov.unscaled
cov.est

var_B1<-cov.est[2,2]
var_B3<-cov.est[4,4]
cov_B13<-cov.est[2,4]


## Cálculo de los ORs del peso de la madre para los distintos nivels de edad
edad_level <- 15:25 
OR   <- rep(NA,length(edad_level))
OR_L <- rep(NA,length(edad_level))
OR_U <- rep(NA,length(edad_level))

i <- 0
for (x in edad_level)
{ 
  i <- i+1 
  SE <- sqrt(var_B1 + x^2 * var_B3 + 2*x *cov_B13)

  OR[i]   <- exp(  B1+x*B3)  
  OR_L[i] <- exp( (B1+x*B3) - 1.96 * SE  )
  OR_U[i] <- exp( (B1+x*B3) + 1.96 * SE  ) 
  
  print ( paste ( "Edad ", edad_level[i] , " OR = ",
                  round( OR[i], dig=2) ,    " (", 
                  round( OR_L[i] , dig=2) , "-" ,
                  round( OR_U[i] , dig=2) , ")", sep ="") )
}
OR
OR_L
OR_U

#####################################
## Gráficos de los ORs de la interacción

## Gráfico 1: 3 OR elegidos
ind <- c(1,6,11) ## elementos de los ORs para dibujar
dev.new()
plot ( c(edad_level[ind], edad_level[ind], edad_level[ind]), 
       c(OR[ind], OR_L[ind], OR_U[ind]), 
       main="ORs (IC95) del peso de la madre en diferentes edades", 
       xlab="edad", ylab="Odds Ratio", pch=16, cex= 1.5)
for ( i in ind ) {segments( edad_level[i], OR_L[i], edad_level[i], OR_U[i]) }     
abline(h=1)


## Gráfico 2:  Todos los ORs con líneas
dev.new()
plot ( edad_level,OR, type="l", ylim=c(min(OR,OR_L,OR_U),max(OR,OR_L,OR_U) ),
       main="ORs (IC95) del peso de la madre en diferentes edades", 
       xlab="edad", ylab="Odds Ratio", lwd=3, lty=1)
lines( edad_level, OR_L, lwd=2, lty=2)  
lines( edad_level, OR_U, lwd=3, lty=2)      
abline(h=1)

     
#####################################
## Interacción entre 2 variables categóricas 

out.6 <- glm ( bajo_pes ~ fumador * peso_gr , data=xx, family = binomial )
summary(out.6)

## Creación de una variable uniendo las 2 categóricas
xx$peso_fuma <- NA
xx$peso_fuma [ xx$fumador == 0 & xx$peso_gr == 0 ] = 0           
xx$peso_fuma [ xx$fumador == 0 & xx$peso_gr == 1 ] = 1    
xx$peso_fuma [ xx$fumador == 1 & xx$peso_gr == 0 ] = 2    
xx$peso_fuma [ xx$fumador == 1 & xx$peso_gr == 1 ] = 3  
xx$peso_fuma <- factor(xx$peso_fuma)
     
out.7 <- glm ( bajo_pes ~ peso_fuma , data=xx, family = binomial )
summary(out.7)      

## ORs
exp( summary(out.7)$coeff[ 2:4, 1] )


