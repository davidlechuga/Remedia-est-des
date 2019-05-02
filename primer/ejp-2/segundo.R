library(readxl)   # libreria para abrir excel
library(dplyr)    # libreria para manegar data frames
library(graphics) #  libreria de grafos
library(ggplot2)  #  libreria de grafos
library(plotrix)  # libreria de grafos


segundo<- read_excel("segundo.xlsx", col_names = c("1","2","3","4"))   # abrimos el excel
attach(segundo)   # habilita las etiquetas del documento
class(segundo)    # vemos la clase del objeto
typeof(segundo)   # vemos el tipo de variable del objeto
Msegundo<- as.matrix(segundo) # convertimos un data frame a una matriz
#Msegundo<-data.frame(sort(Msegundo, decreasing = FALSE))  # los convertimos en una lista
print(Msegundo)   #  imprimimos la matriz
class(Msegundo)   #  clases del objeto
typeof(Msegundo)  #  tipo de variables del objeto
table(Msegundo)   #  de la lista cuantas veces se repiten los numeros (numero de hermanos)
stem(Msegundo,scale = 1)  #  Diagrama de tallo-hoja


na<-data.frame(table(Msegundo))   #volviendo una MATRIX a una TABLA.
colnames(na)<- c("numero de hermanos", "numero de alumnos")   # dandole nombre a las columnas de la tabla.
print(na)  # Imprimir tabla na
sum(table(Msegundo))  #  suma  los que se reptieron (total de numero de alumnos)
b<-data.matrix(na)
plot(na, main="numero de hermanos segun un numero de alumnos", las=1, cex.main=1, cex.lab=1.2, ylim=c(0,12))
points(na, pch=16, lines(na, col="brown"), col="red")


frec.rela.prop<-na[2]/40   #  sacando la frecuencia relativa en proporcion
print(frec.rela.prop)     # imprimiendo frecuencia. relativa. proporcional
frec.rela.porc<-na[2]/40*100  #   frecuencia relativa en porcentaje
print(frec.rela.porc)     # imprimiendo frecuencia. relativa. en porcentaje
sum(frec.rela.porc<-na[2]/40*100)   #suma total de los porcentajes


barplot(table(Msegundo), col = "blue", space = 1, legend.text = FALSE,
        horiz = FALSE, axes = TRUE, axisnames = TRUE, cex.names = 1, cex.axis = 1,
        xlab = "numero de hermanos", ylab = "numero de alumnos", 
        main = "numero de hermanos segun un numero de alumnos", xlim = c(1,18))












