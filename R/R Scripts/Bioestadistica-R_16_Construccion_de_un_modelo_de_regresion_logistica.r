################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##      
## Sesión 16: Construcción de un modelo de Regresión Logística
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
## Paso 1: Análisis Univariante: Test del cociente de verosimilitudes
################################################################################

names(xx)

## Indice de las variables predictoras

ind.var.pred <- c (3, 4, 5, 6, 8, 9, 10, 11) 

for ( ind in ind.var.pred )
{ 
  out.1 <- glm( bajo_pes ~ xx[ , ind] , data=xx, family = binomial ) 
  anova.1 <- anova(out.1 , test="Chisq") 
  print ( paste ( names(xx)[ind] , 
                  round(anova.1$P[2], dig=5) , sep=" " ))
} 

################################################################################
## Paso 2: Modelos multivariantes completo
##         Todas las variables significativas (P<0.25) en el paso anterior
################################################################################

out.2 <- glm( bajo_pes ~ edad + peso + raza + fumador + hta + irr_urin 
                         + part_pre2  , data=xx, family = binomial ) 
summary(out.2)
## LRT Test del cociente de verosimilitudes
drop1(out.2, test="Chisq")

################################################################################
## Paso 3: Modelos multivariantes completo
################################################################################

## Quitando la edad
out.3 <- glm( bajo_pes ~ peso + raza + fumador + hta + irr_urin 
                         + part_pre2  , data=xx, family = binomial ) 
summary(out.3)


## Quitando la irritabilidad urinaria
out.4 <- glm( bajo_pes ~ edad + peso + raza + fumador + hta  
                         + part_pre2  , data=xx, family = binomial ) 
summary(out.4)


################################################################################
## Paso 4: Escala de las variables continuas
################################################################################

## Categorización de Peso
quantile ( xx$peso )
xx$peso4gr <- cut ( xx$peso, breaks = c(quantile(xx$peso)[1:4], 999)  ,
                    labels=1:4 , right=F)
table(xx$peso4gr)
table(xx$peso4gr, xx$peso)

out.5 <- glm( bajo_pes ~ edad + peso4gr + raza + fumador + hta + irr_urin 
                         + part_pre2  , data=xx, family = binomial ) 
summary(out.5)


## Categorización de Edad
quantile ( xx$edad )
xx$edad4gr <- cut ( xx$edad, breaks = c(quantile(xx$edad)[1:4], 999)  ,
                    labels=1:4 , right=F)
table(xx$edad4gr, xx$edad)
table(xx$edad4gr)

out.6 <- glm( bajo_pes ~ edad4gr + peso + raza + fumador + hta + irr_urin 
                         + part_pre2  , data=xx, family = binomial ) 
summary(out.6)


#####################################
## Modelo de efectos principales
#####################################

xx$peso_gr <- NA
xx$peso_gr [ xx$peso < 49.5 ] <- 1
xx$peso_gr [ xx$peso > 49.5 ] <- 0
out.7 <- glm( bajo_pes ~ edad + peso_gr + raza + fumador + hta + irr_urin 
                         + part_pre2  , data=xx, family = binomial ) 
summary(out.7)



################################################################################
## Paso 5: Interacciones
################################################################################

## Explorando las interacciones con peso_gr
set.var <- c ( "edad", "raza", "fumador", "hta" )   ## Variables a explorar en la interacción


# Bucle para cada variable a explorar
for ( name.var in set.var )
{
   ## Se construye un texto con el modelo a evaluar
   modelo <- paste( "bajo_pes ~ edad + raza + fumador + hta + irr_urin + part_pre2 + ",
                    name.var , "* peso_gr" )
   
   ## Modelo con interacción
   out.int <- glm ( as.formula(modelo), data=xx, family = binomial ) 
   
   ## LRT del modelo de efectos principales y el que incluye la interacción
   p.value.inter <- anova ( out.7, out.int, test="Chisq")$"Pr(>Chi)"[2]
   
   print( paste( name.var , " * peso_gr ", round(p.value.inter, dig=4), sep=""))
}


################################################################################
## Paso 5: MODELO FINAL
################################################################################

out.8 <- glm( bajo_pes ~ edad + peso_gr + raza + fumador + hta + irr_urin + part_pre2 +
                edad*peso_gr + peso_gr*fumador , data=xx, family = binomial )
summary(out.8)  


#####################################
## Análisis de influencia. Distancia de Cook
cook.8 <- cooks.distance(out.8)
max( cook.8)

