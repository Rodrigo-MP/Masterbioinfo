################################################################################
################################################################################
## CURSO: Bioestadística con R - Máster de Bioinformática 
##
## Autor: Jesús Herranz
## 
## Sesión 22: Comparaciones múltiples
##           
################################################################################
################################################################################

################################################################################
## Datos simulados          
################################################################################

## Número de observaciones y número de variables
n.ind = 80
num.var = 10000

## Definición de la variable respuesta binaria
var.gr = c ( rep ("A" , n.ind/2 ) , rep ("B" , n.ind/2 ) )
var.gr

## Definición y análisis de una variable independiente
var.x = rnorm ( n.ind , mean = 5 , sd = 2 )

## Medias, SD y t de Student para los 2 grupos
tapply ( var.x , var.gr, mean )
tapply ( var.x , var.gr, sd )
t.test ( var.x ~ var.gr )


## Generamos una matriz con muchas variables con distribuciones normales
yy = matrix ( NA, n.ind , num.var )
yy = apply ( yy , 2 , 
            function ( w.col ) { w.col = rnorm ( n.ind , mean = 5 , sd = 2 ) } )
dim (yy)
yy [1:5 , 1:5 ]

## Test t de Student para todas las variables
p.val = apply ( yy , 2 , function ( w.col ) { t.test ( w.col ~ var.gr )$p.value } )
length(p.val)
head(p.val)
sort(p.val)[1:6]                                        
sum ( p.val < 0.05 )
dev.new(); hist ( p.val , breaks = 20 , main="" )
## Bonferroni. Nivel de significación es alfa/p
0.05 / num.var 
sum ( p.val < 0.05 / num.var )



################################################################################
## Ajuste por Comparaciones múltiples
################################################################################


################################################################################
## Fichero ALL: 12625 variables genéticas y una que define 2 clases (última col)
xx <- read.delim( "C://Bioestadistica con R/Datos/ALL.txt", sep="\t")
dim(xx)
## Número de variables, número de tests
n.var = ncol(xx) - 1

head(names(xx))
tail(names(xx))
table( xx$mol.biol )


########################
## t de Student para cada una de las variables

p.val = apply ( xx[ , - ncol(xx) ] , 2 , 
        function ( w.col ) { t.test ( w.col ~ xx$mol.biol )$p.value } )

## Exploramos los p-values
length(p.val)
sum ( p.val < 0.05 )
dev.new()
hist ( p.val , breaks = 20 , main="" )


################################################################################
## Ajuste por Comparaciones Múltiples

########################
## Bonferroni. Nivel de significación es alfa/p
sum ( p.val < 0.05 / n.var )
w.p.adj.bonf = p.val * n.var                     ## p-valores ajustados  
w.p.adj.bonf [ w.p.adj.bonf > 1 ] = 1
sum ( w.p.adj.bonf < 0.05 )

########################
## Bonferroni
p.adj.bonf = p.adjust ( p.val , method = "bonferroni" )
sum ( p.adj.bonf == w.p.adj.bonf )               ## Chequeamos que coinciden
sum ( p.adj.bonf < 0.05 )
var.sign.bonf = names(xx) [ p.adj.bonf < 0.05 ]  ## Variables significativas
length(var.sign.bonf)
head(var.sign.bonf)

########################
## Benjamini-Hochberg
p.adj.BH = p.adjust ( p.val , method = "BH" )
sum ( p.adj.BH < 0.05 )

## Seleccionar las variables significativas por BH al FDR del 5%
var.sign.BH = names(xx) [ p.adj.BH < 0.05 ] 
length(var.sign.BH)
head(var.sign.BH)

## Explicando BH
m = length(p.val)
d = 0.05
i = 1:m
p.val.sort = sort(p.val)
p.thr = 0.05 * ( i / m )
head(p.val.sort)
head(p.thr)

sum( p.val.sort < p.thr )      ## Contando significativos según BH

## p.ajustados por BH obtenidos manualmente
## Ejemplo
sort(p.val.sort)[1:5]
sort(p.adj.BH)[1:5]

min ( sort(p.val.sort) [1:m]  * ( m / 1:m ) )
min ( sort(p.val.sort) [2:m]  * ( m / 2:m ) )
min ( sort(p.val.sort) [3:m]  * ( m / 3:m ) )
min ( sort(p.val.sort) [4:m]  * ( m / 4:m ) )

## p.ajustados por BH obtenidos manualmente. Proceso
w.p.adj.BH = rep ( NA, m )              ## p-valores ajustados  
for ( k in 1:m )
{ 
  pos = which ( names ( sort(p.val.sort) [k] ) == names(xx) ) 
  w.p.adj.BH [ pos ] = min ( sort(p.val.sort) [k:m]  * ( m / k:m ) ) 
}

w.p.adj.BH [ w.p.adj.BH > 1 ] = 1 
sum ( p.adj.BH == w.p.adj.BH )               ## Chequeamos que coinciden


########################
## q-values

library(qvalue)

qval.out = qvalue ( p.val )
names(qval.out)

## Estimación del FDR si se consideran p-values < 0.01 o 0.001
## Mayor q-value entre todos los que cumple la condición sobre p-values
max ( qval.out$qvalues [ qval.out$pvalues <= 0.01 ] )
max ( qval.out$qvalues [ qval.out$pvalues <= 0.001 ] )

## Seleccionar las variables significativas por qvalue al FDR del 5%
qval.out = qvalue ( p.val  , fdr.level = 0.05 )
table ( qval.out$significant )
max ( p.val [ qval.out$significant == T ] )   ## p-value para significación
var.sign.qval = names(xx) [ qval.out$significant == TRUE ] 
length(var.sign.qval)

## Plot del q-value
dev.new()
plot(qval.out)








