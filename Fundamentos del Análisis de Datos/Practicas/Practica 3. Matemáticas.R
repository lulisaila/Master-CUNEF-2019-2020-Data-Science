###########TAREA 3 FUNDAMENTOS ANALISIS DATOS##############

######################EJERCICIOS MATEMATICAS: LUCIA SAIZ########################

#1. Using D(), find the derivative of 3*x^2 - 2x + 4 ~ x.

library(mosaicCalc)
library(mosaic)

g <- mosaicCalc::D(3*x^2 - 2*x + 4 ~ x)
g # la funcion derivada es f(x) = 3*2*x - 2


#a. Calcular el valor de la derivada en  x = 0
g(0) # el resultado es -2


#b. ¿Como es la grafica de la funcion? 

plotFun(g, x.lim=range(0,10)) # positive sloping line, opcion b



#2. averiguar la derivada de 5*exp(.2*x) ~ x.

l <- mosaicCalc::D(5*exp(.2*x) ~ x)
l # la funcion derivada es 5*exp(0.2*x)*0.2


#a.¿Que vale la derivada cuando x = 0?
l(0) # la derivada de la funcion l vale 1 cuando x = 0


#b. Estudia como estan relacionadas la funcion original y su derivada mediante

# graficos

lorig <- (5*exp(.2*x) ~ x)

plotFun(lorig, x.lim=range(0,10))
plotFun(l, x.lim=range(0,10)) # la respuesta es b, que las dos formulas tienen

# la misma forma pero empiezan con valores iniciales distintos



#3. Utiliza R para sacar la derivada de la funcion exp(-(x^2)) ~ x.Depsues, 

# dibuja la grafica de esa derivada desde x = -2 hasta x = 2. Â¿Como es?

z <- mosaicCalc::D(exp(-(x^2)) ~ x)
z

plotFun(z, x.lim=range(-2, 2)) # la grafica parece una ola creciendo positivamente
# y despues de forma negativa; la respuesta correcta es la c



#4. ¿Cual sera el valor de esta derivada?: D(fred^2 - ginger)

new <- mosaicCalc::D(fred^2 - ginger ~ x)
new

# el resultado es 0 siempre y por tando la opcion a



#5. Utiliza D() para calcular la tercera derivada de cos(2*t). 

tercera <- mosaicCalc::D(cos(2*t) ~ t)

tercera2 <- mosaicCalc::D(-(sin(2 * t) * 2) ~ t)

tercera3 <- mosaicCalc::D(-(cos(2 * t) * 2 * 2) ~ t)

tercera3 # la respuesta es c, 8*sin(2*t)

#¿cual es la cuarta derivada?

tercera4 <- mosaicCalc::D(sin(2 * t) * 2 * 2 * 2 ~ t)
tercera4 # la cuarta derivada es la opcion d, 16*(cos(2*t))



#6. Computar y hacer la grafica de la cuarta derivada de la funcion cos(2*t^2 ~ t)
# desde t = 0 hasta t = 5. ¿Como es la grafica?

cuarta <- mosaicCalc::D(cos(2*t^2) ~ t)

cuarta2 <- mosaicCalc::D(-(sin(2 * t^2) * (2 * (2 * t))) ~ t)

cuarta3 <- mosaicCalc::D(-(cos(2 * t^2) * (2 * (2 * t)) * (2 * (2 * t)) + sin(2 * t^2) * (2 * 2)) ~ t)

cuarta4 <- mosaicCalc::D(-((cos(2 * t^2) * (2 * 2) - sin(2 * t^2) * (2 * (2 * t)) * (2 * (2 * t))) * (2 * (2 * t)) + cos(2 * t^2) * (2 * (2 * t)) * (2 * 2) + cos(2 * t^2) * (2 * (2 * t)) * (2 * 2)) ~ t)
cuarta4

plotFun(cuarta4, x.lim=range(0, 5)) # la respuesta correcta es la c, que la
# amplitud aumenta y el periodo disminuye a medida que crece el valor de t
# las funciones complicadas que aparecen en la cuarta derivada son las que 
# incluye la opcion c; seno, coseno, suma, multiplicacion y cuadrados


#7. Teniendo en cuenta la funcion x*sin(y) que incluyen las variables x e y, 
# crea varias funciones derivadas distintas; una parcial con respecto a x, una
# parcial con respecto a y, la segunda derivada parcial de x e y, y unas parciales
# mezcladas: 

parcialx <- mosaicCalc::D(x*sin(y) ~ x)

parcialy <- mosaicCalc::D(x*sin(y) ~ y)

parcialx2 <- mosaicCalc::D(sin(y) ~ x)

parcialy2 <- mosaicCalc::D(x * cos(y) ~ y)

pxy <- mosaicCalc::D(x*sin(y) ~ x&y)
pyx <- mosaicCalc::D(x*sin(y) ~ y&x)

parcialx(2,3)
parcialy(3,2)

parcialx(1,0)
parcialy(0,1)

# el primer apartado, que dice que las parciales con respecto a x e y son 
# iguales es falso. Usando el mismo valor pero cambiado el orden entre x 
# e y altera el reusltado. 

parcialx2(3,5)
parcialy2(5,3)

parcialx2(2,1)
parcialy2(1,2)

# el segundo apartado, que dice que las segundas parciales con respecto a x 
# e y son iguales, tambien es falso. Usando los mismos valores de x e y, 
# dan resultados distintos. 

pxy(0,1)
pyx(1,0)

pxy(5,3)
pyx(3,5)
# el tercer apartado es verdadero, ya que da igual si se le da le valor mayor
# a x o a y, el resultado va a ser el mismo. 