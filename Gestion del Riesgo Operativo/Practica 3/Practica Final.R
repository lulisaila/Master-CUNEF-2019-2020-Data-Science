################### PRACTICA FINAL

################### LUCÍA SAIZ LAPIQUE

################### 08 de abril de 2020

########## 1. Análisis exploratorio de los datos


# En primer lugar, cargamos las librerias que debemos utilizar
library(MASS)
#install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/R/", type="source")
library(CASdatasets) #Descargar los datos del repositorio si problemas
library(car)
library(actuar) 
library(fitdistrplus)
library(ggplot2)
library(dplyr)
library(skimr)
library(evmix)
library(moments)
library(tidyr)
library(tidyverse)
library(lubridate)

data(danishuni)

# guardamos la base de datos en un objeto que usaremos mas adelante
datos <- danishuni

## agregamos los valores de las pérdidas por semana en dos bases de datos distintas; una de frecuencia y otra de severidad
datos_freq <- datos %>% group_by(Date) %>% summarize(Loss_sev = sum(Loss), Loss_freq = n()) %>%
        complete(Date = seq.Date(min(Date), max(Date), by = "day"))

datos_freq[is.na(datos_freq)] <- 0 ## se incluyen los dias que hay 0 pérdidas

# datos de frecuencia semanales
datos_freq_sem <- datos_freq %>% 
  mutate(week = cut.Date(Date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(Date) %>% 
  group_by(week) %>% 
  summarize(Loss_sev = sum(Loss_sev), Loss_freq = sum(Loss_freq))

# datos de severidad semanales
datos <- datos %>% group_by(Date) %>% summarize(Loss_sev = sum(Loss), Loss_freq = n()) 

datos_sem <- datos %>% 
  mutate(week = cut.Date(Date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(Date) %>% 
  group_by(week) %>% 
  summarize(Loss_sev = sum(Loss_sev), Loss_freq = sum(Loss_freq))


# adjudicamos a distintos objetos para el estudio de severidad y frecuencia
x <- datos_sem$Loss_sev
y <- datos_freq_sem$Loss_freq

### Estudio de frecuencia
summary(y)

mean(y) # como vemos, la media de los datos de la columna en cuestion es 3.768
var(y)  # la varianza es 4.99
median(y)  # la mediana es 3

quantile(y, 0.1)
quantile(y ,seq(0,1, 0.20))
quantile(y, seq(0.9, 1, 0.01))  # vemos que no hay mucha varianza entre los datos que 
# se encuentran en la cola


## visualización de los datos
hist(y, density = 40, breaks = 30, prob = TRUE, xlab = "Frecuencia", main = "Histograma de frecuencia")

## cálculo de momentos
skewness(y)  #coeficiente de asimetria - Asimetría positiva o a la derecha, es decir, que los valores 
#estan principalmente concentrados en la izquierda y los valores que estan en la cola a la derecha de 
#la media la hacen más larga.
kurtosis(y)  #coeficiente de curtosis  - como es mayor que 3, nos indica que la distribución es leptocúrtica, es decir, que
#es más punteaguada. Que sea mas apuntada significa que en la media hay mas probabilidad, pero en las 
#colas hay menos, ya que los datos están más concentrados en la media.


### Estudio de severidad

head(datos_sem$Loss_sev, 10) #vemos los 10 primeros elementos
skim(datos_sem)

sum(is.na(datos_sem))

summary(x)
mean(x) # como vemos, la media de los datos de la columna en cuestion es 13.21709
var(x)  # la varianza es 346.4595
median(x)  # la mediana son 8.971729 

quantile(x, 0.1)
quantile(x ,seq(0,1, 0.20))
quantile(x, seq(0.9, 1, 0.01))  # vemos que hay mucha varianza entre los datos que 
# se encuentran en la cola


## visualización de los datos
hist(x, density = 40, breaks = 30, prob = TRUE,
     xlab = "Pérdidas", main = "Histograma de Severidad")
plot(density(x), main = 'Gráfico de densidad de la distribución')


## cálculo de momentos
skewness(x)  #coeficiente de asimetria mucho más pronunciada que en la distribución de frecuencia
kurtosis(x)  #coeficiente de curtosis  - y también mucho más pronunciada que en la distribución de frecuencia






########### 2. Selección del modelo: inferencia paramétrica



## Selección de la distribución de frecuencia (partimos de la base de que son datos discretos)
###Estimación por Max verosimilitud

##Binomial Negativa
fnbinomMLE <- fitdist(y, "nbinom", method = "mle", discrete = TRUE)
fnbinomMLE$estimate

## Uniforme discreta
funifMLE <- fitdist(y, "unif", method = "mle", discrete = TRUE)
funifMLE$estimate

## Poisson
fpoisMLE <- fitdist(y, "pois", method = "mle", discrete = TRUE)
fpoisMLE$estimate

## Geometrica
fgeomMLE <- fitdist(y, "geom", method = "mle", discrete = TRUE)
fgeomMLE$estimate


### Plot: 
# generamos la curva de cada distribucion con este metodo en el histograma inicial para analizar cual se
# ajusta mejor a los datos
hist(y, pch = 10, breaks = 30, prob = TRUE, main = "Frecuencia de Pérdidas", freq = FALSE,
     xlab = "Pérdidas", ylab = "Densidad")
curve(dnbinom(x, size = fnbinomMLE$estimate[1], mu = fnbinomMLE$estimate[2], log = FALSE), 
      col = "turquoise", lwd = 2, add = T)
curve(dunif(x, funifMLE$estimate[1], funifMLE$estimate[2], log = FALSE), col = "purple", lwd = 2, add = T)
curve(dpois(x, fpoisMLE$estimate, log = FALSE), col = "red", lwd = 2, add = T)
curve(dgeom(x, as.numeric(fgeomMLE$estimate), log = FALSE), col = "green", lwd = 2, add = T)

# Los resultados no son muy concluyentes pero vemos como los picos que se generan con el 
# estudio de todos ellos coinciden levemente con las subidas de los datos y se forma la mayor
# probabilidad por el centro. La Uniforme Discreta no nos aporta mucha informaciÃ³n relevante. 



## cdf-plots:
cdfcomp(list(fnbinomMLE, funifMLE, fpoisMLE, fgeomMLE), xlogscale = FALSE, datapch = ".", 
        datacol = "black", fitlty = 2:5, legendtext = c("Binomial Negativa","Uniforme Discreta", "Poisson", "Geométrica"),
        main = "Comparación de ajustes frecuencia")


## Test de Bondad del ajuste

gofstat(list(fnbinomMLE, funifMLE, fpoisMLE, fgeomMLE), chisqbreaks = c(0:4, 9), discrete = TRUE,
        fitnames = c("Binomial Negativa","Uniforme Discreta", "Poisson", "Geométrica"))



## Conclusión: seleccionamos la binomial negativa




## Ajuste a la distribución de severidad

## Como primer análisis, vemos que los valores siguen una distribución continua. Debido al estudio de 
# momentos realizado y teniendo en cuenta que debemos aplicar ajustes a distribuciones continuas, 
# deducimos que, debido a los altos valores en los coeficientes de curtosis y de asimetria, y que los 
# valores atípicos se encuentran a la derecha de la distribución, decidimos ajustar los datos a una 
# distribución exponencial y una log normal (Gamma y Weibull)


###Estimación por bondad del ajuste

## Exponencial
fexponMGE <- fitdist(x, "exp", method = "mge", gof = "CvM")
fexponMGE$estimate

## Log normal
flnormMGE <- fitdist(x, "lnorm", method = "mge", gof = "CvM")
flnormMGE$estimate

## Pareto
fparMGE <- fitdist(x, "pareto", method = "mge", gof = "CvM")
fparMGE$estimate

## Gamma
fgamMGE <- fitdist(x, "gamma", method = "mge", gof = "CvM")
fgamMGE$estimate


## Weibull
fweiMGE <- fitdist(x, "weibull", method = "mge", gof = "CvM")
fweiMGE$estimate

## Burr
fburrMGE <- fitdist(x, "burr", start = list(shape1 = 2, shape2 = 2, scale = 2), lower = c(0.1,1/2, 0), method = "mge", gof = "CvM")
fburrMGE$estimate


### Plot:
#generamos la curva de cada distribucion con este metodo en el histograma inicial para analizar cual se
# ajusta mejor a los datos
hist(x, pch = 10, breaks = 30, prob = TRUE, main = "Distribuciones de severidad", freq = FALSE, xlab = "Pérdidas", ylab = "Densidad")
curve(dexp(x, fexponMGE$estimate, log = FALSE), col = "turquoise", lwd = 2, add = T)
curve(dlnorm(x, flnormMGE$estimate[1], flnormMGE$estimate[2], log = FALSE), col = "purple", lwd = 2, add = T)
curve(dpareto(x, fparMGE$estimate[1], fparMGE$estimate[2], log = FALSE), col = "red", lwd = 2, add = T)
curve(dgamma(x, fgamMGE$estimate[1], fgamMGE$estimate[2], log = FALSE), col = "green", lwd = 2, add = T)
curve(dweibull(x, fweiMGE$estimate[1], fweiMGE$estimate[2], log = FALSE), col = "yellow", lwd = 2, add = T)
curve(dburr(x, fburrMGE$estimate[1], fburrMGE$estimate[2], fburrMGE$estimate[3], log = FALSE), col = "blue", lwd = 2, add = T)


## qq-plots:

qqcomp(list(fexponMGE, flnormMGE, fparMGE, fgamMGE, fweiMGE, fburrMGE), xlogscale = TRUE, ylogscale = TRUE, 
       ylab = "cuantiles empíricos", xlab="cuantiles teóricos", main="QQ-plot sobre pérdidas", addlegend = TRUE, 
       legendtext = c("Exponencial","Log-Normal", "Pareto", "Gamma", "Weibull", "Burr"), xlegend = 'topleft',fitpch=1:4)

## pp-plots:
ppcomp(list(fexponMGE, flnormMGE, fparMGE, fgamMGE, fweiMGE, fburrMGE), xlogscale = TRUE, ylogscale = TRUE, 
       ylab = "cuantiles empíricos", xlab="cuantiles teóricos", main="PP-plot sobre pérdidas", addlegend = TRUE, 
       legendtext = c("Exponencial","Log-Normal", "Pareto", "Gamma", "Weibull", "Burr"),fitpch=1:4)


## cdf-plots:
cdfcomp(list(fburrMGE, fexponMGE, flnormMGE, fparMGE, fgamMGE, fweiMGE), xlogscale = TRUE, datapch = ".", 
        datacol = "black", fitlty = 2:5, legendtext = c("Burr","Exponencial","Log-Normal", "Pareto", "Gamma", "Weibull"),
        main = "Comparación de ajustes")



### Estimación por Máxima Verosimilitud

## Exponencial
fexpMLE <- fitdist(x, "exp", method = "mle")
fexpMLE$estimate

### Lognormal
flnormMLE <- fitdist(x, "lnorm", method = "mle")
flnormMLE$estimate

## Pareto
fparMLE <- fitdist(x, "pareto", method = "mle")
fparMLE$estimate

## Gamma
fgamMLE <- fitdist(x, "gamma", method = "mle")
fgamMLE$estimate

### Weibull
fweiMLE <- fitdist(x, "weibull", method = "mle")
fweiMLE$estimate

## Burr
fburrMLE <- fitdist(x, "burr", start = list(shape1 = 2, shape2 = 2, scale = 2), lower = c(0.1,1/2, 0), method = "mle")
fburrMLE$estimate



### Plot: generamos la curva de cada distribucion con este metodo en el histograma inicial para analizar cual se
# ajusta mejor a los datos
hist(x, pch = 10, breaks = 30, prob = TRUE, main = "Pérdidas", freq = FALSE, xlab = "X", ylab = "Densidad")
curve(dexp(x, fexpMLE$estimate, log = FALSE), col = "turquoise", lwd = 2, add = T)
curve(dlnorm(x, flnormMLE$estimate[1], flnormMLE$estimate[2], log = FALSE), col = "purple", lwd = 2, add = T)
curve(dpareto(x, fparMLE$estimate[1], fparMLE$estimate[2], log = FALSE), col = "red", lwd = 2, add = T)
curve(dgamma(x, fgamMLE$estimate[1], fgamMLE$estimate[2], log = FALSE), col = "green", lwd = 2, add = T)
curve(dweibull(x, fweiMLE$estimate[1], fweiMLE$estimate[2], log = FALSE), col = "yellow", lwd = 2, add = T)
curve(dburr(x, fburrMLE$estimate[1], fburrMLE$estimate[2], fburrMLE$estimate[3], log = FALSE), col = "blue", lwd = 2, add = T)

## qq-plots:

qqcomp(list(fexpMLE, flnormMLE, fparMLE, fgamMLE, fweiMLE, fburrMLE), xlogscale = TRUE, ylogscale = TRUE, 
       ylab = "cuantiles empíricos", xlab = "cuantiles teóricos", main = "QQ-plot sobre pérdidas", addlegend = TRUE, 
       legendtext = c("Exponencial","Log-Normal", "Pareto", "Gamma", "Weibull", "Burr"), xlegend = 'topleft',fitpch = 1:4)

## pp-plots:

ppcomp(list(fexpMLE, fburrMLE, flnormMLE, fparMLE, fgamMLE, fweiMLE), xlogscale = TRUE, ylogscale = TRUE, 
       ylab = "cuantiles empíricos", xlab = "cuantiles teóricos", main = "PP-plot Comparación Ajuste Severidad", addlegend = TRUE, 
       legendtext = c("Exponencial", "Burr", "Log-Normal", "Pareto", "Gamma", "Weibull"),fitpch = 1:4)

## cdf-plotS:
cdfcomp(list(fexpMLE, flnormMLE, fparMLE, fgamMLE, fburrMLE, fweiMLE), xlogscale = TRUE, datapch = ".", 
        datacol = "black", fitlty = 3:10, legendtext = c("Exponencial","Log-Normal", "Pareto", "Gamma", "Burr","Weibull"),
        main = "Comparación de ajustes")



## Test de Bondad del ajuste

gofstat(list(fexpMLE, flnormMLE, fparMLE, fgamMLE, fweiMLE, fburrMLE), chisqbreaks = c(0:4, 9), discrete = FALSE,
        fitnames = c("Exponencial","Log-Normal", "Pareto", "Gamma", "Weibull", "Burr"))

gofstat(list(fexponMGE, flnormMGE, fparMGE, fgamMGE, fweiMGE, fburrMGE), chisqbreaks = c(0:4, 9), discrete = FALSE,
        fitnames = c("Exponencial","Log-Normal", "Pareto", "Gamma", "Weibull", "Burr"))

## Conclusión: Conclusión: Primero, comentar que ambos métodos ajustan de forma muy similar. Elegimos la distribución de Burr, 
#que es a la q más se adecúa, y que lo vemos tanto en los gráficos como en los tests de bondad del ajuste, aunque la Log-normal 
# se acerca mucho. 



##################### 3. Análisis de valores extremos:


### Block máxima

maximos <- datos %>% 
  mutate(week = cut.Date(Date, breaks = "1 week", labels = FALSE)) %>% 
  arrange(Date) %>% 
  group_by(week) %>% 
  summarize(loss_max = max(Loss_sev))

danish.max <- maximos$loss_max
danish.max

quantile(danish.max, 0.1)
quantile(danish.max, seq(0,1, 0.20))
quantile(danish.max, seq(0.9, 1, 0.01))  
# Vemos que hay mucha varianza entre los datos que se encuentran en la cola, y a ojo es difícil seleccionar el umbral. 

## para seleccionar el umbral, calculamos el rango intercuartil
q1 <- quantile(danish.max, 0.25)
q3 <- quantile(danish.max, 0.75)

## rango = diferencia entre tercer cuartil y primer cuartil
rq <- q3 - q1

##Para calcular los valores extremos tanto a la derecha como a la izquierda, calculamos lo siguiente:
dcha <- q3 + 1.5*rq 
izq <- q1 - 1.5*rq  
dcha
izq

## Este es el umbral que seleccionamos, pues a la izquierda no hay valores extremos
u <- dcha


# Siguiente paso
danish.exc <- danish.max[danish.max > u] # nuexa extraccion con los valores que superan el umbral que hemos introducido

danish.exc # salen 59 valores de nuestra extraccion

n.u <- length(danish.exc) #n de casos que superan u
n.u

#Det. prob empiricas de la muestra. Generamos un ranking para estudiar la probabilidad de que aparezca cada uno de esos valores extremos

surv.prob <- 1 - (rank(danish.exc)/(n.u + 1))
#Se obtiene la probabilibada como 1 menos cociente entre nº orden/61.
#El valor 263.25 tiene la prob mas baja de ocurrencia

surv.prob ## vemos que la probabilidad de ocurrencia de la posicion 4, es decir, del valor mas alto de nuestros datos,
# es extremadamente pequeña. Las probabilidades son acumuladas, y representan la probabilidad de que se den esas cuantias una vez
# que se supere el umbarl proporcionado.

plot(danish.exc, surv.prob, log = "xy", xlab = "Excesos", ylab = "Probabilidades", ylim = c(0.01, 1))




### Distribución de pérdidas agregadas

#Parámetros de distribuciones seleccionadas
severidad <- fburrMLE$estimate
frecuencia <- fnbinomMLE$estimate
  

xmax <- qburr(1 - 1e-9, severidad[1], severidad[2], severidad[3]) #Func.cuantialica  

#Calculamos función de distribucion discretizando la distribución de severidad y agregando con la distribución de frecuencia 
#como vemos a continuación (Discretización por M): recursivo

#Metodo insesgado (Seleccionamos este porque los otros dos generaban errores)
fx2 <- discretize(pburr(x, severidad[1], severidad[2], severidad[3]), from = 0,
                  to = xmax, step = 0.5, method = "unbiased", 
                  lev = levburr(x, severidad[1], severidad[2], severidad[3])) #unbiased# metodo centrado


F2 <- aggregateDist("recursive", model.freq = "negative binomial",
                    model.sev = fx2, size = frecuencia[1], mu = frecuencia[2], 
                    prob = 2/3, x.scale = 0.5, maxit = 2000)

## Visualizaciones de las pérdidas agregadas:

plot(sort(x), sort(F2(x)), type = "l", main = "Distribución Agregada de pérdidas", ylab = "F1(y)")

plot(F2, do.points = F, verticals = T, col = 'blue')


## Simulación


set.seed(432)  # For reproducibility of results

m <- 10000      # Number of observations to simulate


# Guardamos los parámetros de la distribución de frecuencia para la primera parte de la simulación a generar:
size <- fnbinomMLE$estimate[1]
mu <-  fnbinomMLE$estimate[2]   # Parameter for frequency distribution N


# Y los de severidad
shape1 <- fburrMLE$estimate[1]
shape2 <- fburrMLE$estimate[2]
scale <- fburrMLE$estimate[3]  # Parameters for severity distribution X
S <- rep(NA, m) # Initalize an empty vector to store S observations

n <- rnbinom(m, size = size, mu = mu) # Generate m=10000 observations of N from Poisson
for(j in 1:m){ 
  n_j <- n[j] # Given each n_j (j=1,...,m), generate n_j observations of X from uniform
  x_j <- rburr(n_j, shape1 = shape1, shape2 = shape2, scale = scale)
  s_j <- sum(x_j) # Calculate the aggregate loss s_j
  S[j] <- s_j # Store s_j in the vector of observations
}

#A continiuación, nombramos la media y desviación típica que usaremos en el último apartado. 
media <- mean(S) 
desv <- sd(S)  

######################## PLOTS #############################

##Ploteamos la densidad de la distribución de frecuencia, despuúes la de la distribución de severidad, y por último la densida de la distribución generada por la simulación, 
##habiendo combinado las dos anteriores. 

semanales <- data.frame(datos_sem)

semanales %>% ggplot(aes(x = Loss_freq)) + geom_density(fill = "darkseagreen1", color = "#e9ecef", alpha = 1) + xlab("Frecuencia")
semanales %>% ggplot(aes(x = Loss_sev)) + geom_density(fill = "deepskyblue", color = "#e9ecef", alpha = 1) + xlab("Cuantía pérdidas")

agregado <- data.frame(S[S > 0])

agregado %>% ggplot(aes(x = S[S > 0])) + geom_density(fill = "turquoise", color = "#e9ecef", alpha = 1) + xlab("Pérdidas agregadas")



################# 5. Apéndice: medición del riesgo extremo:Value at Risk (VaR)

library(sn)

## datos pérdidas agregadas:

## Asumiendo una distribución Normal

#vaR
alpha <- c(0.95)

# cálculo del VaR
vaR <- -media + desv * qnorm(alpha)
vaR

#Es
ES <- (-media + desv * dnorm(qnorm(alpha))/(1 - alpha))
ES


x <- seq(0.9, 0.999, length = 100)

yVaR <- (-media + desv * qnorm(x)) 
yES <- (-media + desv * dnorm(qnorm(x))/(1 - x)) 


## Ploteamos todos los alphas posibles de 90%-100%
plot(x, yVaR, type = "l", ylim = range(yVaR, yES),
     xlab = 'Posibilidades de porcentaje', ylab = "Coronas Danesas", main = 'Representación del VaR y ES')
lines(x, yES, lty = 2, col = 2)


## EXTRA
hist(datos_sem$Loss_sev, breaks = 500, freq = FALSE)
q <- quantile(datos_sem$Loss_sev, probs = 0.95)
abline(v = q, col = 2)

hist(datos_sem$Loss_sev, breaks = 500, freq = FALSE)
q <- quantile(datos_sem$Loss_sev, probs = 0.99)
abline(v = q, col = 2)



                                                                                                             