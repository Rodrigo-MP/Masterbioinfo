################################################################################
################################################################################
## CURSO: Bioestad�stica con R - M�ster de Bioinform�tica 
##
## Autor: Jes�s Herranz
## 
## Sesi�n 04: Gr�ficos en R      
##                                   
################################################################################
################################################################################

##########################################
## Funci�n plot()
                                                
x <- c(2,4,7,8,9,10,12,13,15,16,17,20)
y <- c(78,56,82,103,116,103,132,137,123,116,147,140)

## Grafico simple
dev.new()
plot(x, y)

## Gr�fico con par�metros
dev.new()
plot(x, y, main="Titulo", sub="Subtitulo", xlab="Tiempo", ylab="Reacci�n",
           xlim=c(0,20), pch=16, cex=1.5)

## Gr�fico con par�metros 
dev.new()
plot(x, y, main="Titulo", sub="Subtitulo", xlab="Tiempo", ylab="Reacci�n",
           cex.main = 1.5, cex.lab=0.8, font.sub=4,   
           type="l", lwd=7, col="red", 
           xlim=c(0,20), xaxp=c(0,20,10) )


## Funci�n par() 
dev.new()
par(col="red", pch=3)
plot(x,y)

dev.new()
par(mfrow=c(2,2))
plot(x,y)
hist(x)
boxplot(x)
hist(y)


## Funciones gr�ficas de bajo nivel
z <- c(73,87,91,99,107,123,122,141,130,144,137,151)
dev.new()
plot(x, y, type="l", lwd=3, col="red" )
lines(x, z, lwd=3, col="blue" )
legend(17,70, leg=c("Serie Y","Serie Z"), c("red","blue"), cex=0.7)
abline(h=100)


## Salvar gr�ficos
## Formato PDF
pdf("C://Bioestadistica con R/Temp/Graficos 1.pdf")
plot(x, y, type="l", lwd=3, col="red"  )                     
plot(x, z, type="l", lwd=3, col="blue" )
dev.off()

## Formato JPG (solo salva el �ltimo)
jpeg("C://Bioestadistica con R/Temp/Graficos 2.jpg" , quality=100)
plot(x, z, type="l", lwd=3, col="blue" )
dev.off()


