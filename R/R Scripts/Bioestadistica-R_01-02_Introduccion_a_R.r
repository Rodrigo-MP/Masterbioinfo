################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##    
## Sesión 01: Introducción a R
## Sesión 02: Variables y objetos de R           
##
################################################################################
################################################################################
    
##########################################
## Ayuda

?mean
help(mean)

help.search("mean")
?stats::weighted.mean
example("mean")

##########################################
## Librerías

search()
library(survival)
search()

##########################################
## Expresiones y asignaciones

6 + 3
a <- 6 + 3
a
A
A <- 3
a
A

b <- 3 ; c = 4
d <-
6
b
c
d

##########################################
## Objetos y Workspace

ls()
rm(b, c, d)
ls()

## Salvar todo
save.image("C:/Bioestadistica con R/Temp/Ejemplo.RData")
## Salvar un objeto
save( a, file="C:/Bioestadistica con R/Temp/Ejemplo2.RData")

                   
load("C:/Bioestadistica con R/Temp/Ejemplo.RData")
ls()

##########################################
## Carpetas y directorios

getwd()
setwd("C:/Bioestadistica con R/Temp")
getwd()
setwd("C:/Bioestadistica con R")
getwd()


################################################################################
## Vectores
################################################################################

a <- c(1,5,7,8)
a
assign("b", c(9,5,3,2))
b
a+b

a[2]
a[5] 

c<-c(4,6,9)
a+c

##########################################
## Secuencias

1:20
n<-5
1:n
1:(n-1)

seq(1,10, by=2)

rep(1:3, 2)

mean.res <- rep(NA,20)
mean.res

##########################################
## Funciones básicas

x<-c(3.3, 3.2, 1.7, 2.3, 4.5, 2.6)
min(x)
max(x)
which.min(x)
which.max(x)
which( x == 3.2 )
which( x > 3 )
c(1.7, 1.8) %in% x
sum(x)
mean(x)
sd(x)
sort(x)
order(x)
rank(x)

##########################################
## Missing

x <- c( 3, NA, 6, 2, 4, 6, NA, 1)
x
is.na(x)
sum(is.na(x))    ## Número de missings en x
mean(x)
mean(x,  na.rm=T)

##########################################
## Caracteres

a <- c("Hola", "Adiós")
b <- "Hoy es un buen día"
length(a)
length(b)

paste( "Me dijo", a[1], "y yo le respondí", a[2])
paste( "Me dijo", a[1], "y yo le respondí", a[2], sep="-" )
paste( "Me dijo", a[1], "y yo le respondí", a[2], sep="" )

grep("es",b)
grep("xx",b)
substring ( "Me dijo Hola" , 4, 7 )

print( a, b )
print(a)

cat( a, b )
cat( a, b, "\n")

##########################################
## Indexación

a <- c (3,2,1,2,5,6,NA,4,5,NA,4,8,NA,3,7)
a[1]
a[c(1,3,8)]
a[4:7]
a[a>4]
is.na(a)
a[is.na(a)==FALSE]
a[!is.na(a)]
a[!is.na(a) & a>4]
a[-(1:2)]


################################################################################
## Arrays
################################################################################

a <- 1:24
a
dim(a) <- c(4,6) 
a
a[1,2]
a[1,]



x <- array ( NA, c(2,5))
x
y <- array ( 1:2, c(2,2,2))
y
x <- matrix ( 1:10, 2, 5 )
x
dim(x)
nrow(x)
ncol(x)
t(x)
diag(x)

rbind(1:3, 6:8, 10:12)                     

################################################################################
## Listas
################################################################################

a <- 4
b <- c(1,2,3)

list1 <- list ( a, b, "Hola")
list1                                                
list1[[1]]
list1[[2]]
list1[[2]] [3]    ## 3º elemento del 2º componente

names(list1) <- c("var", "vector", "texto")
list1
list1$vector

## Alternativa  (así se usa con los resultados de las funciones)
list2 <- list(var=a, vector=b, texto="Hola")
list2

