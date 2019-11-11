################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
## 
## Sesión 18-19: Regresión de Cox
##
################################################################################
################################################################################

################################################################################
## Librerías
################################################################################

library(survival)

################################################################################
## Fichero Datos: whas500
################################################################################

xx <- read.csv(file="C://Bioestadistica con R/Datos/whas500.csv", sep=";")
dim(xx)
head(xx)


################################################################################
## Regresión de Cox
################################################################################

##########################
## Variable continua
cox1 <- coxph ( Surv(lenfol, fstat) ~ age, data=xx )
summary(cox1)

## cambio de escala
c <- 10

summary(cox1)$coeff
summary(cox1)$coeff[1]
exp(summary(cox1)$coeff[1])

exp( c * summary(cox1)$coeff[1] )
exp( c * summary(cox1)$coeff[1] - 1.96 * abs(c) * summary(cox1)$coeff[3] )
exp( c * summary(cox1)$coeff[1] + 1.96 * abs(c) * summary(cox1)$coeff[3] )

##########################
## Supuesto de Riesgos Proporcionales

cox.zph(cox1)
dev.new()
plot(cox.zph(cox1))


##########################
## Variable binaria
cox2 <- coxph ( Surv(lenfol, fstat) ~ chf, data=xx )
summary(cox2)


##########################
## Variable categórica

quantile(xx$age, , right=F)
xx$age4gr <- cut( xx$age, breaks = c(quantile(xx$age)[1:4], 999)  ,
                  labels=1:4 , right=F )
table(xx$age4gr)
sum(table(xx$age4gr))
                           
cox3 <- coxph ( Surv(lenfol, fstat) ~ age4gr, data=xx )
summary(cox3)

##########################
## Variable categórica

quantile(xx$bmi, , right=F)
xx$bmi4gr <- cut( xx$bmi, breaks = c(quantile(xx$bmi)[1:4], 999)  ,
                  labels=1:4 , right=F )
table(xx$bmi4gr)
sum(table(xx$bmi4gr))

cox3 <- coxph ( Surv(lenfol, fstat) ~ bmi4gr, data=xx )
summary(cox3)


##########################
## Modelo Multivariante de Efectos Principales

cox4 <- coxph ( Surv(lenfol,fstat) ~ age + hr + diasbp + bmi + gender + chf,
                                        data=xx)                
summary(cox4)


##########################
## Test del cociente de verosimilitudes, quitando “gebder”

cox8 <- coxph ( Surv(lenfol,fstat) ~ age + hr + diasbp + bmi + chf, data=xx)                
anova(cox8, cox4, test="Chisq")
drop1(cox4, test="Chisq")


##########################
## Modelo Multivariante con Interacciones

cox5 <- coxph ( Surv(lenfol,fstat) ~ age + hr + diasbp + bmi + gender + chf 
                                        + age*gender, data=xx)
summary(cox5)


###################################################
## Interacción: HR del Sexo en diferentes edades

B1 <- summary(cox5)$coef[1] ## age
B2 <- summary(cox5)$coef[5] ## gender
B3 <- summary(cox5)$coef[7] ## age*gender

cox5$var

var_B2  <- cox5$var[5,5]  ## varianza gender
var_B3  <- cox5$var[7,7]  ## varianza age*gender
cov_B23 <- cox5$var[5,7]  ## covarianza gender - age*gender

age_set <- c(40,50,60,70,80,90)

HR <- rep(NA,length(age_set))
HR_L <- rep(NA,length(age_set))
HR_U <- rep(NA,length(age_set))

i<-0
for (x in age_set)
{
  i<-i+1
  SE      <- sqrt(var_B2 + x^2 * var_B3 + 2*x *cov_B23)
  
  HR[i]   <- exp(  B2+x*B3)  
  HR_L[i] <- exp( (B2+x*B3) - 1.96 * SE  )
  HR_U[i] <- exp( (B2+x*B3) + 1.96 * SE  )
  print ( paste ( round( HR[i], dig=2) ,    " (", 
                  round( HR_L[i] , dig=2) , "-" ,
                  round( HR_U[i] , dig=2) , ")", sep ="") )  
}


#####################
## Gráfico de los HRs de la interacción

dev.new()
plot ( c(age_set, age_set, age_set), c(HR, HR_L, HR_U), 
       main="HRs (IC95%) del sexo en diferentes edades", 
       xlab="edad", ylab="Hazard Ratio", pch=16, cex= 1.5)
for ( i in 1:length(HR)) 
  {segments( age_set[i], HR_L[i], age_set[i], HR_U[i]) }     
abline(h=1)


################################################################################
## Análisis de influencia. Estadístico de desplazamienzo de la Verosimilitud
################################################################################

## Likelihood Displacement
surv5  <- survreg(cox5, data=xx)
res_ld <- residuals(surv5, type="ldcase")   
max(res_ld)



################################################################################
## Prediciendo la supervivencia con el modelo de Cox
################################################################################

######################################################
## Supervivencia predicha por el modelo de Cox para 2 individuos

## Individuo con profile 1 (hombre de 70 años, línea negra)
dev.new()
surv_profile1 <- survfit ( cox5, newdata=data.frame(age = 70 , hr = 85 , diasbp = 80, 
                             bmi = 25, gender = 0 , chf = 0 ), se=F ) 
plot( surv_profile1, mark.time=F, conf.int=F, col="black", lwd=2 )   

## Individuo con profile 2 (mujer de 40 años, línea roja)
surv_profile2 <- survfit ( cox5, newdata=data.frame(age = 40 , hr = 85 , diasbp = 80, 
                             bmi = 25, gender = 1 , chf = 0 ), se=F ) 
lines( surv_profile2, mark.time=F, conf.int=F, col="red", lwd=2 )   

summary(surv_profile2)


################################################################################
## Modelo de Regresión de Cox Estratificado
################################################################################

cox10 <- coxph ( Surv(lenfol, fstat) ~ sho + age + chf, data=xx )
summary(cox10)

cox.zph(cox10)

dev.new(); par (mfrow=c(1,3))
for ( i in 1:3 )  plot(cox.zph(cox10), var=i)

## Modelo de Cox estratificado
cox11 <- coxph ( Surv(lenfol, fstat) ~ strata(sho) + age + chf, data=xx )
summary(cox11)

dev.new()
plot ( survfit ( cox11 ), col=c("red","blue"), mark.time=F, lwd=2 )  
legend ( 500, 0.2 , legend=c("sho_0","sho_1"), c("red","blue"))

## Modelo de Cox estratificado, que incluye las interacciones
cox12 <- coxph ( Surv(lenfol, fstat) ~ strata(sho) + age + chf + 
                                       sho:age + sho:chf, data=xx )
summary(cox12)

