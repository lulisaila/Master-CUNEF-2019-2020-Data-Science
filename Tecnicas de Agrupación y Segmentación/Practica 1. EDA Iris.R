################ Autor: Lucia Saiz Lapique

############# Asignatura: Tecnicas de agrupacion y segmentaicon

########## Ejercicio: Tarea 1. EDA base de datos IRIS


## Cargamos las librerias

library(readr)
library(ggplot2)
library(tidyverse)
library(skimr)
library(dplyr)
library(corrplot)

## cargamos los datos en cuestion

iris <- read_csv("iris.csv")

## Breve resumen de los datos y renombramos las variables

skim(iris)
names(iris)[1] <- "Sepal Length"
names(iris)[2] <- "Sepal Width"
names(iris)[3] <- "Petal Length"
names(iris)[4] <- "Petal Width"

## Visualizamos los datos

head(iris)
hist(iris$`Sepal Length`)

## Seleccionamos las variables numericas para hacer un estudio de los datos

iris_num <- select(iris, -Species)

datos_stats = data.frame( 
  Min = apply(iris_num, 2, min, na.rm = TRUE), # valor minimo de la variable
  Q1 = apply(iris_num, 2, quantile, 1/4, na.rm = TRUE), # 1er cuartil
  Med = apply(iris_num, 2, median, na.rm = TRUE), # mediana de la variable
  Mean = apply(iris_num, 2, mean, na.rm = TRUE), # media de la variable
  Q3 = apply(iris_num, 2, quantile, 3/4, na.rm = TRUE), # 3er cuartil
  Max = apply(iris_num, 2, max, na.rm = TRUE) # valor maximo de la variable
)
datos_stats = round(datos_stats, 1) # redondeo de los datos con un decimal
datos_stats
  

## Visualizacion de las relaciones entre variables cuantitativas con variable cualitativa

ggplot(iris, aes(x = Species, y = `Sepal Length`, fill = Species)) +
  geom_bar(stat = "identity") + 
  xlab("Variety") +
  ylab("Sepal Length") 

ggplot(iris, aes(x = Species, y = `Sepal Width`, fill = Species)) +
  geom_bar(stat = "identity") +
  xlab("Variety") +
  ylab("Sepal Width") 

ggplot(iris, aes(x = Species, y = `Petal Length`, fill = Species)) +
  geom_bar(stat = "identity") +
  xlab("Variety") +
  ylab("Petal Length") 

ggplot(iris, aes(x = Species, y = `Petal Width`, fill = Species)) +
  geom_bar(stat = "identity") +
  xlab("Variety") +
  ylab("Petal Width") 


## Visualizacion de relación entre las variables cuantitativas

ggplot(iris, aes(x = iris$`Sepal Width`, y = `Sepal Length`, color = `Sepal Width`)) +
  geom_point(stat = "identity") +
  xlab("Sepal Width") +
  ylab("Sepal Length") 


ggplot(iris, aes(x = iris$`Petal Width`, y = `Petal Length`, color = `Petal Width`)) +
  geom_point(stat = "identity") +
  xlab("Petal Width") +
  ylab("Petal Length") 


ggplot(iris, aes(x = iris$`Petal Width`, y = `Sepal Width`, color = `Petal Width`)) +
  geom_point(stat = "identity") +
  xlab("Petal Width") +
  ylab("Sepal Width") 

ggplot(iris, aes(x = iris$`Petal Length`, y = `Sepal Length`, color = `Petal Length`)) +
  geom_point(stat = "identity") +
  xlab("Petal Length") +
  ylab("Sepal Length") 

ggplot(iris, aes(x = iris$`Petal Width`, y = `Sepal Length`, color = `Petal Width`)) +
  geom_point(stat = "identity") +
  xlab("Petal Width") +
  ylab("Sepal Length") 

ggplot(iris, aes(x = iris$`Petal Length`, y = `Sepal Width`, color = `Petal Length`)) +
  geom_point(stat = "identity") +
  xlab("Petal Length") +
  ylab("Sepal Width") 


## Estudio de correlación de variables numericas

cor.mat = round(cor(iris_num, use = "complete.obs"), 2)  # buscamos los valores de correlacion entre las variables
# graficamos las correlaciones
corrplot(cor.mat, type = "full", order = "hclust", addrect = 3,
         tl.col = "black", tl.srt = 45)



