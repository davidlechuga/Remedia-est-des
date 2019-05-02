library(readxl) #leer archivos en excel
library(dplyr)  # libreria para manipular frames
library(graphics)  #  libreria de grafos
primero <- read_excel("primero.xlsx", col_names = FALSE)  #abrimos el documento
typeof(primero)  # vemos el tipo de variables del objeto
View(primero)
primero1<-as.matrix(primero)  # convertimos una lista en una matriz 
typeof(primero1) # vemos el tipo de variables del objeto
sort(primero1, decreasing = FALSE)  #  ordenamos los resultados de menor a mayor.
segundo<-data.frame(sort(primero1, decreasing = FALSE))  # los convertimos en una lista
typeof(segundo)  #  vemos el tipo de variables del objeto
segundo1<-as.matrix(segundo)  # convertimos una lista en una matriz 
typeof(segundo1)  #  emos el tipo de variables del objeto
colnames(segundo)<-c("edades")  #  le agregamos un nombre a la columna.
colnames(segundo1)<-c("edades")
length(segundo1)  # vemos el tamaño del vector


lugardelamediana<-length((segundo1)+1)/2   #  lugar de la mediana
print(segundo1[15:16])  # los lugares de la mediana son 15,y 16
(46+47)/2 # el valor de la mediana es  46.5

stem(segundo$edades)  # we crearte a steam- and -leaf-diagram of edades

tabulate(segundo$edades) # de 1-80 cuantas veces se repitio el numero ?
table(segundo$edades)  # solo de los resultados cuales se repitieron?

