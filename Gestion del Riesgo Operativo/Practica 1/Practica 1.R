################  GESTION RIESGO OPERATIVO ###########

################    Practica1_fraude   ###########

################    Lucia Saiz Lapique     #############3



library(readr)
library(moments) #Package para calculo de momentos (asimetria, curtosis, ...)
library(actuar) #Package para analisis actuarial
library(fitdistrplus) #Package para ajuste de distribuciones
library(ggplot2) #Package para visulacion
library(purrr)  #Package para ciertas distribuciones
library(stats)


x <- read_csv("practica_propuesta1.csv")  # cargamos los datos
names(x)[1] = paste("Fraudes")  # nombramos la columna de la base de datos de nuevo Fraudes
# porque no tengo claro que son los datos



################### Analisis de los datos


head(x$Fraudes, 10) #vemos los 10 primeros elementos
summary(x$Fraudes)  # resumen de los datos
table(x$Fraudes) #tabla de frecuencias

skewness(x$Fraudes)  #coef. de asimetria - Asimetria negativa o la izquierda
kurtosis(x$Fraudes)  #coef. de curtosis  - Platicurtica o menos apuntada Normal, coeficiente es menor de 3




#Tambien:

mean(x$Fraudes) # como vemos, la media de los datos de la columna en cuestion es 114,09
var(x$Fraudes)  # la varianza es 210,36
median(x$Fraudes)  # la mediana son 115 

quantile(x$Fraudes, 0.1)

quantile(x$Fraudes,seq(0,1, 0.20))

quantile(x$Fraudes,seq(0.9,1, 0.01))

hist(x$Fraudes, pch = 20, breaks = 25, prob = TRUE, main = "HISTOGRAMA",
     xlab = " Nº Fraudes", ylab = "Frecuencia") # El histograma me permite ver la distribucion 
# por encima de los datos para poder elegir como estudiarlos. La distribución es discreta, así
## que solo la podemos estudiar en base a distribuciones discretas. 


# Con esta informacion debemos intentar ajustar una distribución de probabilidad.
# A continuacion revisaremos los posibles modelos para ver cual se ajusta mas a los datos

#Ajuste maxima verosimilitud de distribuciones discretas


fpois = fitdist(x$Fraudes, "pois") #Ajuste de una Poisson
fpois #Obtenemos el valor estimado de Lambda y de la desviacion.
plot(fpois) # Visualizacion tanto de los datos empiricos como de los estimados


fnbinom = fitdist(x$Fraudes, distr = "nbinom") #Ajustamos una Binomial Negativa
fnbinom #Obtenemos el valor estimado de los dos parametros y la desviacion de cada uno de los valores.
plot(fnbinom)

unif = fitdist(x$Fraudes, "unif") # Ajustamos una uniforme discreta
unif
plot(unif)

geom = fitdist(x$Fraudes, "geom") # Ajustamos una distribucion geometrica
geom
plot(geom)



#Ajuste por bondad del ajuste

gofstat(list(fpois, fnbinom, unif, geom), chisqbreaks = c(0:4, 9), discrete = TRUE,
        fitnames = c("Poisson", "Binomial Negativa", "Uniforme Discreta", "Geometrica"))



# Comentarios sobre las distribuciones:
# Se obtienen los valores del estadistico Chi-cuadrado ademas de los grados de libertad de cada
# distribucion.

# En cuanto a los valores de Pvalor, como todos son mayores que 0,05, se acepta la hipotesis 
# nula y establecemos que la muestra en cuestion pertenece a todos estos tipos de distribucion
# (en principio)

# La tabla con los valores empíricos y los teóricos para ambas distribciones nos muestra que los valores mas bajos
# pertenecen a la distribucion binomial negativa y a la gamma, con lo cual reducimos los posibles
# tipos de distribucion a esas dos (la siguiente mas baja es la de Poisson). 

# Finalmente, se eligira aquel modelo con el menor valor de AKAIKE y de Bayesian C, que es la Binomial 
# Negativa

# Como en este caso el Pvalor es menor que el nivel de signifacion, no se puede rechazar la hipotesis,
# como hemos dicho anteriormente.




