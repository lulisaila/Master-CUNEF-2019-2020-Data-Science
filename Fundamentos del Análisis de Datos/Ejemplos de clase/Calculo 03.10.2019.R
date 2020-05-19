###########MATEMATICAS ESENCIALES PARA EL DATA SCIENCE##############

######################FUNDAMENTOS DE CALCULO########################


######  Derivadas

#Cargar MosaicCalc desde CRAN
#Activar
library(mosaicCalc)
library(mosaic)


#1. Calculo de la derivada x^2 y evaluaci?n en distintos puntos

g = mosaicCalc::D(x^2 ~ x)

# Obtenemos la funcion
g 

g(1/2)
g(0)
g(-1)

#dibuja el grafico incluyendo rango
plotFun(g, x.lim = range(0,10)) 


#2. Valoraci?n de funciones en un punto

h = makeFun(2*x^2 - 5*x + 2 ~x) 
h(x = 2)
h(x = 5)

#dibuja el grafico incluyendo rango

plotFun(h(x) ~x,x.lim = range(-5, 5)) 


#3. Calculo de la derivada y evaluaci?n en dif. puntos

H = mosaicCalc::D(2*x^2 - 5*x + 2 ~x) 

H 
H(1)
H(4)

plotFun(H(x) ~x, x.lim = range(-5, 5))



#4. Calculo de la derivada y evaluaci?n en dif. puntos

j = mosaicCalc::D(x^3 - x ~x) 
j  

j(1)
j(4)

plotFun(j(x) ~x, x.lim = range(-5, 5))



# OTRA FORMA

A = symbolicD((x^3 - x) ~x)  
A

B = symbolicD(x^4 ~x)
B


# 5. Segundas derivadas


j = mosaicCalc::D(x^3 - x ~x) #Primera derivada
j

dj = mosaicCalc::D(j(x) ~x) #Segunda derivada
dj



# 6.  Funciones de varias variables


mosaicCalc::D(4 * x^2 * sin(y) ~ x)


mosaicCalc::D(4 * x^2 * sin(y) ~ x + x)  #Segundas derivadas


mosaicCalc::D(4 * x^2 * sin(y) ~ x + y) 



#Derivada del producto Pag.20
b = mosaicCalc::D((x^3 - x)*(5*x^4 + x^2) ~x)  
b

#Derivada de cociente
e = mosaicCalc::D(1/(x-2) ~x)  
e


# Algunos ejemplos pag. 24
k = mosaicCalc::D((x^3 + x^2)^50 ~x)
k

L = mosaicCalc::D(((x - 1)/(x + 3))^(1/3) ~x)
L


########   Optimizacion


#Cargar nloptr desde Packages
#Activar
library(nloptr)


f = function(x) x*sin(4*x)


curve(f, 0, 3)

curve(x*sin(4*x), 0, 3) #sustituir 


#Ofrece el primer min. encontrado

optimize(f, c(0, 3), lower = 0, upper = 3) 

#Ofrece el min. global

optimize(f, c(0,3), lower = 2, upper = 3) 

#ofrece el max.
optimize(f, c(0,3), lower = 0, upper = 3, maximum = T) 


#Estudiar la concavidad/convexidad

ff = mosaicCalc::D(x*sin(4*x) ~x) #Primera derivada
ff

dff = mosaicCalc::D(ff(x) ~x) #Segunda derivada
dff

curve(dff, c(-10, 10))


####

g = function(x) x*(20 - 2*x)*(16 - 2*x)
curve(g, 0, 10)

optimize(g, c(0,10), lower = 0, upper = 10, maximum = T)
optimize(g, c(0,10), lower = 0, upper = 10) 

#Estudiar la concavidad/convexidad
gg = mosaicCalc::D(x*(20 - 2*x)*(16 - 2*x) ~x) #Primera derivada
gg

dgg = mosaicCalc::D(gg(x) ~x) #Segunda derivada

curve(dgg, c(-10, 10))


#Estudiar la concavidad/convexidad  pag.59
h = function(x) x^2 - 2*x + 2
curve(h, 0, 100)

hh = mosaicCalc::D(h(x) ~x) #Primera derivada
hh

dhh = mosaicCalc::D(hh(x) ~x) #Segunda derivada
dhh #>0 para todo x ---- h es convexa


