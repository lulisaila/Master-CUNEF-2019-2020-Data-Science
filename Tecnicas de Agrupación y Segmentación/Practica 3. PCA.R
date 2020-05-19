############# PRÁCTICA 3

######### Descripción base de datos ACPTITUD

############ Lucía Saiz Lapique


## Cargamos las librerías necesarias
library(tidyverse)
library(factoextra)
library(FactoMineR)
library(skimr)
library(corrplot)
library(readr)
library(dplyr)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(psych)

library(factoextra)
library(FactoMineR)


datos <- read_delim("C:/Users/Luli/Desktop/TODO/GitHub/lulisaila-Master_Data_Science_CUNEF/Tecnicas de Agrupación y Segmentación/BBDD/ACPTIUSD.csv", 
                    ";", escape_double = FALSE, trim_ws = TRUE)
datos <- na.omit(datos)

# Realizamos un summary completo del archivo,
## Descripcion de contenido de las variables
## Respecto de cada uno: minimo, maximo, cuartiles y media

summary(datos)
skim(datos)

## Distribucion de cada variable. Generamos un histograma con la curva de densidad para cada una de ellas

hist(datos$`DEPO 1M` , density = 25, breaks = 15, prob = TRUE,
     xlab = "Intereses",
     main = "Histograma deposito a 1 mes")
curve(dnorm(x, mean = mean(datos$`DEPO 1M`), sd = sqrt(var(datos$`DEPO 1M`))),
      col = "turquoise", lwd = 2, add = TRUE, yaxt = "n")

hist(datos$`DEPO 3M` , density = 25, breaks = 15, prob = TRUE,
     xlab = "Intereses",
     main = "Histograma deposito a 3 meses")
curve(dnorm(x, mean = mean(datos$`DEPO 3M`), sd = sqrt(var(datos$`DEPO 3M`))),
      col = "turquoise", lwd = 2, add = TRUE, yaxt = "n")


hist(datos$`DEPO 6M` , density = 25, breaks = 15, prob = TRUE,
     xlab = "Intereses",
     main = "Histograma deposito a 6 meses")
curve(dnorm(x, mean = mean(datos$`DEPO 6M`), sd = sqrt(var(datos$`DEPO 6M`))),
      col = "turquoise", lwd = 2, add = TRUE, yaxt = "n")


hist(datos$`DEPO 12M` , density = 25, breaks = 15, prob = TRUE,
     xlab = "Intereses",
     main = "Histograma deposito a 12 meses")
curve(dnorm(x, mean = mean(datos$`DEPO 12M`), sd = sqrt(var(datos$`DEPO 12M`))),
      col = "turquoise", lwd = 2, add = TRUE, yaxt = "n")


hist(datos$`IRS 2Y` , density = 25, breaks = 15, prob = TRUE,
     xlab = "Intereses",
     main = "Histograma preddiccion a 2 a?os")
curve(dnorm(x, mean = mean(datos$`IRS 2Y`), sd = sqrt(var(datos$`IRS 2Y`))),
      col = "turquoise", lwd = 2, add = TRUE, yaxt = "n")


hist(datos$`IRS 3Y` , density = 25, breaks = 15, prob = TRUE,
     xlab = "Intereses",
     main = "Histograma preddiccion a 3 a?os")
curve(dnorm(x, mean = mean(datos$`IRS 3Y`), sd = sqrt(var(datos$`IRS 3Y`))),
      col = "turquoise", lwd = 2, add = TRUE, yaxt = "n")


hist(datos$`IRS 4Y` , density = 25, breaks = 15, prob = TRUE,
     xlab = "Intereses",
     main = "Histograma preddiccion a 4 a?os")
curve(dnorm(x, mean = mean(datos$`IRS 4Y`), sd = sqrt(var(datos$`IRS 4Y`))),
      col = "turquoise", lwd = 2, add = TRUE, yaxt = "n")


hist(datos$`IRS 5Y` , density = 25, breaks = 15, prob = TRUE,
     xlab = "Intereses",
     main = "Histograma preddiccion a 5 a?os")
curve(dnorm(x, mean = mean(datos$`IRS 5Y`), sd = sqrt(var(datos$`IRS 5Y`))),
      col = "turquoise", lwd = 2, add = TRUE, yaxt = "n")


hist(datos$`IRS 7Y` , density = 25, breaks = 15, prob = TRUE,
     xlab = "Intereses",
     main = "Histograma preddiccion a 7 a?os")
curve(dnorm(x, mean = mean(datos$`IRS 7Y`), sd = sqrt(var(datos$`IRS 7Y`))),
      col = "turquoise", lwd = 2, add = TRUE, yaxt = "n")


hist(datos$`IRS 10Y` , density = 25, breaks = 15, prob = TRUE,
     xlab = "Intereses",
     main = "Histograma preddiccion a 10 a?os")
curve(dnorm(x, mean = mean(datos$`IRS 10Y`), sd = sqrt(var(datos$`IRS 10Y`))),
      col = "turquoise", lwd = 2, add = TRUE, yaxt = "n")


# correlacion entre las variables
matriz_cor <- cor(datos[,-1])
matriz_cor

det(matriz_cor) 
# cercano a 0, indica alta multicolinealidad entre las variables. Si fuese igual a 0, 
# diríamos que la matriz no es singular). Supuesto de multicolinealidad: test de esfericidad de Bartlett busca 
# contrastar la hipótesis nula de que la matriz de correlaciones es igual a una matriz 
# de identidad. No hay homogeneidad de varianzas. 

rcorr(as.matrix(datos[,c(-1, -11)]))

correlacion <- round(cor(datos[,-1]), 1)  # p valor
correlacion

## graficamos la correlación entre todas las variables para hacer un análisis visual de todas
corrplot(correlacion, method = "number", type = "upper")


bartlett.test(datos[,-1])  # como el resultado del p-valor es mucho menor que 0.05, existe
# multicolineadlidad en el conjunto de variables.
# Mediante el test de bartlett buscamos contrastar la hipotesis nula de que la matriz 
# de correlaciones es igual a la matriz de identidad. A efectos de multicolinealidad, nos
# interesa rechazar la hipotesis nula y aceptar la alternativa de que la matriz es 
# distinta a la de identidad, y por tanto existe suficiente multicolinealidad entre las 
# variables 

KMO(matriz_cor)
# otra prueba de multicolinealidad
# 0,90 > KMO Muy bueno 0,90 > KMO > 0,80 Bueno 0,80 > KMO > 0,70 Aceptable 
# 0,70 > KMO > 0,60 Mediocre o regular 0,60 > KMO > 0,50 Malo 0,50 > KMO Inaceptable 
# o muy malo. Autovalores y autovectores de la matriz de covarianzas de la muestra.
# HAy algo debajo que esta explicando esas variables, factores comunes 


# Generamos objeto ACP con orden PCA (base). Ese objeto se genera a partir de una orden. 
# Extraemos una columna especifica escribiendo [,-1], de forma que elegimos solo la 
# columna 1. Hacemos esto porque es la unica variable de caracteristica caracter.
# Análisis de componentes principales:
acp <- PCA(datos[,-1], graph = T) 

# Grafico de la puntuacion de las variables en el espacio: va a estar siempre en dos dimensiones, independientemente de los valores con 
# los que decidamos trabajar al final, el resultado siempre va a estar representado en 
# dos dimensiones. Eliminamos la fecha ya que sabemos que es una variable informativa 
# acerca de las distintas dimensiones que hay. Hay una asociacion entre las distintas 
# caracteristicas. No hace falta explicar la fecha, sino las caracteristicas asociadas 
# a cada bono. El 100% de la informacion, de la varianza, queda recogido en esas 10 
# dimensiones. 

acp$eig # con FacotMineR
get_eig(acp) #con factoextra

fviz_eig(acp, addlabels = TRUE, hjust = -0.3) +
  labs(title = "Screen plot / Grafico de sedimentacion", x = "Dimensiones", y = "% Varianza explicada") +
  theme_minimal()

var = get_pca_var(acp) #factoextra
var

acp

# coordenadas y contribuciones de las variables
var$contrib  # contribuciones (en %) de las variables a los CCPP. La contribucion de una variable (var) 
# a un CP dado es (en %): (var.cos2 * 100) / (total cos2 del componente).

var$cor # correlaciones de las observaciones (ind) o variables (var)

var$cos2 # representa la calidad de la representacion para las variables sobre el mapa factorial. 
# Se calcula como el cuadrado de las coordenadas: var.cos2 = var.coord * var.coord

corrplot(var$cos2, is.corr = FALSE) 
corrplot(var$contrib, is.corr = FALSE)

var$coord #coordenadas de las observaciones (ind) o variables (var)
var$coord^2

# con las varianzas y el grafico de barras anterior, deducimos que con las dos 
# primeras dimensiones podemos obtener la mayor cantidad de informacion posible con 
# el menor numero de variables. 

# Representacion grafica de la correlacion donde introducimos tambien la contribucion.
# El codigo de colores me da la tercera dimension. Colores: cuanto as fria, menos 
# contribucion y cuanto as caliente,mas contribucion.

fviz_pca_var(acp, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) +
  labs(title = "Mapa de ejes principales", subtitle = "Contribuciones") +
  theme_minimal()

fviz_pca_var(acp, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE) +
  labs(title = "Mapa de ejes principales", subtitle = "Cos cuadrado") +
  theme_minimal()

# Contribucin de las variables a cada uno de los ejes principales
## eje 1
fviz_contrib(acp, choice = "var", axes = 1 ) +
  labs(title = "Contribuciones a la Dim 1")

## eje 2
fviz_contrib(acp, choice = "var", axes = 2 ) +
  labs(title = "Contribuciones a la Dim 2")

## ambos ejes
fviz_contrib(acp, choice = "var", axes = 1:2) +
  labs(title = "Contribuciones a las dos dimensiones")

####


acp_g = fviz_pca_ind(acp, geom = "point",
                    addEllipses = TRUE,
                    ellipse.level = 0.986) + 
  labs(title = "Puntuaciones de las observaciones en las dimensiones") +
  theme_minimal()
print(acp_g) 


varimax(acp$var$cor[,1:2])

# corto y largo plazo 

