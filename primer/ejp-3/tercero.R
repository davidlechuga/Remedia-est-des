library(readxl)   # libreria para abrir excel
library(dplyr)    # libreria para manegar data frames
library(graphics) #  libreria de grafos
library(ggplot2)  #  libreria de grafos
library(plotrix)  # libreria de grafos
#install.packages("fdth") #  instalamos paqueteria para el calculo de distribuciones de frecuencia
#install.packages("agricolae")
library(fdth)
library(agricolae)


tercero<- read_excel("tercero.xlsx", col_names = c("1","2","3","4","5"))   # abrimos el excel
#attach(tercero)   # habilita las etiquetas del documento
#class(tercero)    # vemos la clase del objeto
#typeof(tercero)   # vemos el tipo de variable del objeto
Mtercero<- as.matrix(tercero) # convertimos un data frame a una matriz
Mtercero<-as.vector(Mtercero)
Mtercero
#Msegundo<-data.frame(sort(Msegundo, decreasing = FALSE))  # los convertimos en una lista
#print(Mtercero)   #  imprimimos la matriz
#class(Mtercero)   #  clases del objeto
#typeof(Mtercero)  #  tipo de variables del objeto
#table(Mtercero)   #  de la lista cuantas veces se repiten los numeros (numero de hermanos)
stem(Mtercero,scale = 1)  #  Diagrama de tallo-hoja
mtercero<- fdt(Mtercero,breaks = "Sturges")
mtercero


par(mfrow=c(3,2)) # particiona mi ventana grafica en 3x2.


hist(Mtercero, breaks = "sturges", ylim = c(0,21), col = "skyblue1", xlab = "Ancho de la variable discontinua (Sturges)", 
     ylab = "Frecuencia absoluta", 
     main = " Numero de Alumnos con esa calificación", labels = TRUE) #histograma utilizando el numero de clases según Sturges.


plot(mtercero, type="fh", xlab = "Ancho de la variable discontinua (Límites Reales)", 
     ylab = "Frecuecia absoluta", col = "tomato3", 
     main = "Numero de Alumnos con esa calificación")   ###  Histograma con la frecuencia absoluta y con limites de clase.


plot(mtercero, type="fp", ylab = "Frecuencia absoluta", 
     xlab = "Ancho de la variable discontinua (Limites Reales)",
     main = "Numero de Alumnos con esa calificación (poligono de frecuencias o Gauss Suavizada)",
     cex.main="0.85", pch=15, col = "darkgreen")  ### Poligono de Frecuencia o campana de Gauss Suavizada con Frecuencia Absoluta y con Límites Reales


desa<-c(0,50,48,46,41,29,12,3)    #  Se toman los valores de la frecuencia desacumulada
desa<-desa/50*100             #  Los valores en des se dividen por N   y se  multiplica por 100
desa<-sort(desa,decreasing = TRUE)    #   Se ordenan de mayor a menor los %
desa                         #  imprime valores de des

aa<- c(37.5,46.5,55.5,64.5,73.5,82.5,91.5,100.5)    # Se crea un string con valores del ancho del intervalo "eje x"
aa
bb<-c(0,4,8,18,42,76,94,100)       # Se crea un string con valores de la frecuencia acumulada en %
bb

plot(aa, bb, type = "o", pch=15, col="darkorchid",
     ylim = c(0,110), xlim=c(0,110),
     main = "Porcentaje de Alumnos con esa calificación",
     ylab = "Frecuencia absoluta y desacumulada en %",
     xlab = "ancho de la Variable discontinua")
lines(aa,desa,type="o",pch=15, col="black")     #   Se crea un Polígono porcentual con las frecuencias desacumlada y acumulada.





#range(Mtercero)   # Recorrido de la vriable
#rango<- 98-38     # valor del rango   max - min 
#a<-log10(50)      #  operacion de logaritmo base 10 (N)
#b<-a*3.22+1       #  multiplicacion  1 + 3.322 log (N)
#c<-rango/b        #  divición R A N G O / 1 + 3.322 LOG (N)  El resultado se redonde a 9
#numclas<-(rango+1)/9  # Se encuentran los numeros de intervalo o clases (R A N G O + 1) / i  **   ** Y  SE   R  E  D O N  D  E  A  "   7  "
#numclas



#data=sample(38:100,50,replace=F)
#data
#m=matrix(data,ncol=5,nrow=10)
#c=cut(data,breaks=c(38,46,55,64,72,80,88,96))
#w=table(c)
#w


x<-c(61,33,35,74,48,53,53,48,23,19,15,57,72,40,27,25,75,66,34,27,38,16,42,57,27,59,37,63,71,48)   # se crea una cadena de numeros
#x
#sort(x, decreasing = FALSE)   # ordena la cadena de numero de menor a mayor
#range(x)          # se encuentran los valores max y  min   de la cadena de numeros
#rango<- 75-15     #  se encuentra el rango restando valor max - min 
#length(x)         #  el tamaño de la cadena de numero
#a<-log10(30)      #  operacion de logaritmo base 10 (N)
#b<-a*3.22+1       #  multiplicacion  1 + 3.322 log (N)
#c<-rango/b        #  divición R A N G O / 1 + 3.322 LOG (N)  El resultado se redonde a 9

dist <- fdt(x,breaks="Sturges") # calcula la distribución de frecuencias utilizando la regla Sturge
dist

par(mfrow=c(3,2)) # particiona mi ventana grafica en 3x2.

hist(x, breaks = "Sturges", ylim = c(0,6), col = "skyblue1", xlab = "Ancho de la variable discontinua (Sturges)", 
     ylab = "Frecuencia absoluta", 
     main = " Numero de Alumnos con esa calificación", labels = TRUE) #histograma utilizando el numero de clases según Sturges.

plot(dist, type="fh", xlab = "Ancho de la variable discontinua (Límites Reales)", 
     ylab = "Frecuecia absoluta", col = "tomato3",
     main = "Numero de Alumnos con esa calificación")   ###  Histograma con la frecuencia absoluta y con limites de clase.

plot(dist,type = "cfh", ylim = c(0,33), main = "Numero de Alumnos con esa calificacion",
     ylab = "Frecuencia absoluta acumulada", xlab = "Ancho de la variable discontinua (Límites Reales)",
     col = "gold2")   ## Histograma con frecuencia absoluta acumulada y con limites reales.


plot(dist, type="fp", ylab = "Frecuencia absoluta", 
     xlab = "Ancho de la variable discontinua (Limites Reales)",
     main = "Numero de Alumnos con esa calificación (poligono de frecuencias o Gauss Suavizada)",
     cex.main="0.85", pch=15, col = "darkgreen")  ### Poligono de Frecuencia o campana de Gauss Suavizada con Frecuencia Absoluta y con Límites Reales


plot(dist, type="cfpp", xlab = "Ancho de la variable discontinua (Limites Reales)", 
     ylab = "Frecuencia Absoluta Acumulada en % ", 
     main = "Numero de Alumnos con esa Calificacion",
     col = "darkorchid", pch=15, ylim = c(0,120)) 
    ###   Ojiva de la Frecuencia Absoluta Acumulada en % con Límites  Reales


des<-c(4,11,15,20,25,30)    #  Se toman los valores de la frecuencia desacumulada
des<-des/30*100             #  Los valores en des se dividen por N   y se  multiplica por 100
des<-sort(des,decreasing = TRUE)    #   Se ordenan de mayor a menor los %
des                         #  imprime valores de des

xx<- c(14.85,25,35.15,55.45,65.60,75.75)    # Se crea un string con valores del ancho del intervalo "eje x"
xx
dd<-c(13.33,36.67,50,66.67,83.33,100)       # Se crea un string con valores de la frecuencia acumulada en %
dd

plot(xx, dd, type = "o", pch=15, col="darkorchid",
     ylim = c(0,110), xlim=c(0,110),
     main = "Porcentaje de Alumnos con esa calificación",
     ylab = "Frecuencia absoluta y desacumulada en %",
     xlab = "ancho de la Variable discontinua")
lines(des,dd,type="o",pch=15, col="black")     #   Se crea un Polígono porcentual con las frecuencias desacumlada y acumulada.


