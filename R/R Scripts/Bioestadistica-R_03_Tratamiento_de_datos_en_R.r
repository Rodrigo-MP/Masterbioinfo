################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
##          
## Sesión 03: Tratamiento de datos con R
##
################################################################################
################################################################################


################################################################################
## Data frames
################################################################################

df <- data.frame( ID=c(1,3,4,5,8,9,10,11), edad=c(34,46,23,19,23,11,14,34),
                  sexo=c("H","M","M","M","H","M","H","M") ) 
df
df$edad
mean(df$edad)
df[ , c("edad","sexo")]
df[ , 2:3]
names(df)


df$tratamiento <- c(0,0,0,1,1,0,1,0)
df

## Funciones con data frames 
dim(df)
head(df)
tail(df)

names(df)
names(df)[2]


## Orden merge

df1 <- data.frame( ID =c(1,2,3,4), edad=c(12,34,44,54) )
df2 <- data.frame( ID2=c(1,2,3,4), hta=c(0,1,0,1) ) 
df3 <- merge(df1, df2, by.x="ID", by.y="ID2" )
df3

df2 <- data.frame( ID2=c(1,2,4,5), hta=c(0,1,0,1) ) 
df3 <- merge(df1, df2, by.x="ID", by.y="ID2" )
df3
df3 <- merge(df1, df2, by.x="ID", by.y="ID2", all=T )
df3
df3 <- merge(df1, df2, by.x="ID", by.y="ID2", all.x=T )
df3
df3 <- merge(df1, df2, by.x="ID", by.y="ID2", all.y=T )
df3

## Uniendo filas con rbind

df1 <- data.frame( ID =c(1,2,3,4), edad=c(12,34,44,54) )
df2 <- data.frame( ID =c(5,6), edad=c(42,28) )
df3 <- rbind(df1, df2)
df3


################################################################################
## Factores
################################################################################

df <- data.frame( ID=c(1,3,4,5,8,9,10,11), edad=c(34,46,23,19,23,11,14,34),
                  sexo=c("H","M","M","M","H","M","H","M") ) 
df$tratamiento <- c(0,0,0,1,1,0,1,0)
is.factor(df$edad)
is.factor(df$sexo)
is.factor(df$tratamiento)

df$sexo
df$tratamiento
levels(df$tratamiento)

df$tratamiento <- as.factor(df$tratamiento)                  
df$tratamiento
levels(df$tratamiento)

#########################
## Algunos problemas con Factores
x <- c(0,0,0,0,0,1,1,1,1,1,1,1,1,1,2,2,2,2)
x<-factor(x)
table(x)
levels(x)

## Asigna valores 1,2,3 .... a las categorías ordenadas
as.integer(x)
x.num <- as.integer( as.character(x) )    ## Truco
x.num

## Guarda los niveles siempre

x [ x == 2 ] <- 1
x
table(x)
x = as.factor ( as.character( x ))
x


################################################################################
## Lectura de ficheros
################################################################################

f1 <- read.table(file="C://Bioestadistica con R/Ficheros para importar/Ejemplo 1.txt",
                 header=T)
dim(f1)
head(f1)


f2 <- read.csv(file="C://Bioestadistica con R/Datos/Bajo peso al nacer.csv", sep=";")
dim(f2)
head(f2)


################################################################################
## Importación de ficheros de otros paquetes estadísticos
################################################################################

library(foreign)
library(xlsx)

#######################
## SPSS
f3<-read.spss("C://Bioestadistica con R/Ficheros para importar/Ejemplo 3.sav",
              to.data.frame=TRUE)
head(f3)

#######################
## STATA

f4<-read.dta("C://Bioestadistica con R/Ficheros para importar/Ejemplo 4.dta")
head(f4)              

#######################
## EXCEL - 2 hojas

f5<-read.xlsx ( "C://Bioestadistica con R/Ficheros para importar/Ejemplo 5.xlsx", 
                sheetIndex=1 )
head(f5) 
f6<-read.xlsx ( "C://Bioestadistica con R/Ficheros para importar/Ejemplo 5.xlsx", 
                sheetIndex=2 )
head(f6) 


################################################################################
## Ficheros de salida                                             
################################################################################

## Salvamos a fichero de texto un dataframe
write.table ( f4, "C://Bioestadistica con R/Temp/Ejemplo 4.txt", 
            quote=FALSE , sep="\t", col.names=TRUE, row.names=FALSE)

## Se crea un fichero de salida
FileOut <- file("C://Bioestadistica con R/Temp/Resultados 4.csv", "w")

cat ( "Variable;Media;SD;Median", file=FileOut, sep="\n")
cat ( paste ("Edad", mean(f4$edad, na.rm=T), sd(f4$edad, na.rm=T), 
              median(f4$edad, na.rm=T), sep=";") 
     , file=FileOut, sep="\n")  
                                                         
close(FileOut)
         
           

