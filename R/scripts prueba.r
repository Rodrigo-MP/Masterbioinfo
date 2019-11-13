x = c ( 4, 10, 11, 13, 15, 16, 17, 18, 19, 20, 22, 24, 25, 27, 29, 30, 31 )
for ( i in 1:length(x) )
{
  if ( x [i] >= 20 )
  (break)
  { break }
  { print ( x[i] ) }
}
x

xx <- read.csv(file="/home/rodrigo/github/Masterbioinfo/R/Datos/Bajo peso al nacer.csv", sep=";")

names(xx)
xx$bajo_pes <- factor(xx$bajo_pes)
