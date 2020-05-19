##### APUNTES DE CLASE 07/11/2019. Distancias
## LUCÍA SAIZ LAPIQUE

library(readr)

## Cargamos los datos
Dist <- read_delim("C:/Users/Luli/Desktop/TODO/GitHub/lulisaila-Master_Data_Science_CUNEF/Tecnicas de Agrupación y Segmentación/BBDD/EMDDistancias.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Dist

# le quitamos la primera columna
dist <- Dist[,-1] 
dist

# convertimos la base de datos en una matriz particular:
dist2 <- as.matrix(dist)
dist2

# creo un objeto de ciudad con la primera columna que excluimos al principio
ciudad <- Dist[,1]
ciudad 

# transformamos matriz de distancias, donde tenia las distintas ciudades con sus
# respectivas distancias, las situo en un mapa donde las tendre en funcion de su
# distancia relativa
# necesito saber cuales son las coordenadas en el grafico de las ciudades y posteriormente
# le voy a pegar las etiquetas de su nombre a cada coordenada. 
d <- as.dist(as(dist, "matrix"))
d # va cruzando Albacete con todas y pone todas las distancias. Todo lo que hay
# en la diagonal de la matriz ppal. 
# El objeto d que hemos creado es ahora una matriz de distancias, no una cualquiera

# obtengo los cuadrados de cada una de las distancias generando la matriz de 
# cuadrados. Todas las distancias euclideas (distancia "ordinaria" entre dos 
# puntos de un espacio eucl?deo, la cual se deduce a partir del teorema de 
# Pit?goras) trabajan con cuadrados
dist_cuad <- dist2 * dist2
dist_cuad

# matriz de doble centrado: matriz identidad (Mj) N * N, a la que se le resta
# 1 / n (numero de variables) (en nuestro caso un septimo). Matriz de unos de
# dimension n * n. 
# Objetivo: obtener la matriz "P"
# Primero obtengo la matriz identidad 7x7:
ident <- diag(7)

# creo un vector con unos, repetidos 7 veces. Me crea un vector con fila de 7 unos que replico 7 veces
uno <- rep(1, 7) 
Uno <- rep(uno, each = 7)  # vector fila de 49 unos

dim(Uno) = c(7, 7) # matriz 7x7 a partir del vector de unos
Uno

# Objetivo: J = ident - 1/n Uno
# la divido en dos partes:
jota_1 <- 1/7 * Uno
jota <- ident - jota_1

jota

# Aplicamos el doble centrado:
# Producto matricial
B_1 <- jota %*% dist_cuad %*% jota
B <- -1/2 * B_1

round(B, 3)
B # matriz cuadrada 7x7 

# sacamos los autovectors y los autovalores de nuestra matriz 
AV <- eigen(B)
AV

AV$vec
AV$values  

# no me interesan todos los autovalores y autovectores, solo los mayores dos
# creamos objetos reducidos para la solucion de dos dimensiones  
AV_red <- AV$values[1:2] # le pedimos los dos mayores autovalores reducidos (los primeros)
AV_red

AVvect_red <- AV$vec[, 1:2] # pedidmos los dos mayores autovectores reducidos (los primeros)
AVvect_red

# Generamos la mtriz de coordenadas: voy a pasar de un espacio de 7x7 a uno de 
# dos dimensiones ?nicamente. Cada ciudad se refiere a otras seis, cada una
# est? posicionada con respecto a otras seis. Quiero pasar esa informaciona un
# espacio de dos dimensiones. 
# Necesito las coordenadas en el nuevos sistema de dos dimensiones. 
# Cuando paso a mas de tres es ijmposible representarlo en dos dimensiones

# para obtener las coordenadas: necesito mi vector de autovalores reducido 
# (formado por lambda 1 y lambda 2) y la matriz E, formada por e1 y e2 (AVvect_red)
# que voy a multiplicar por una matriz diagonal con lo que vamos a obtener la matriz 
# de coordenadas
AV_red_mat <- sqrt(AV_red)*diag(2) # matriz diagonal con los autovalores reducidos de antes
X <- AVvect_red %*% AV_red_mat # premultiplicar por los autovectores

X # reultado: matriz de coordenadas 

X <- as.data.frame(X) # convertirlo en dataframe
colnames(X) <- c("H", "V") # cambiar los nombres a coordenada horizontal y vertical

X

# data frame de posiciones donde Albacete esta 
posicion <- as.data.frame(cbind(ciudad, X))
posicion

H <- posicion$H
H

V <- posicion$V
V

plot(H, V,
     main = "Posiciones relativas",
     xlab = "Dimension 1",
     ylab = "Dimension 2",
     col = "blue", pch = 19, cex = 1, lty = "solid", lwd = 2,
     frame.plot = FALSE)
text(H, V, labels = ciudad, cex = 0.7, pos = 3)
abline(h = c(0), v = (0), lty = 2, col = "blue")


# nueva base de datos para hacerlo de otra forma
corr <- read.csv("C:/Users/Luli/Desktop/TODO/GitHub/lulisaila-Master_Data_Science_CUNEF/Tecnicas de Agrupación y Segmentación/BBDD/EMDCorr.csv", header = TRUE, sep = ";")
corr2 <- corr[, -1]
corr2

var <- corr[, 1]

str(var)
str(corr2)

d <- as.dist(as(corr2, "matrix"))
d

# libreria de escalado multidimensional
# MDS clasico o analisis de coordenadas ppales
ms <- cmdscale(dist(corr))
ms

dist(ms)
dim(ms)

plot(ms[,1], ms[,2], xaxt = "n", yaxt = "n",
     xlab = "Dimension 1",
     ylab = "Dimension 2", 
     type = "p",
     bty = "n",
     col = "blue", pch = 19, cex = 1, lty = "solid", lwd = 2,
     frame.plot = FALSE)
axis(side = 1, at = seq(-1, 1.5, 0.5), lwd = 3)
axis(side = 2, at = seq(-0.5, 0.5, 0.25), lwd = 3)
text(ms[,1], ms[,2], labels = var, cex = 0.6, pos = 3)
abline(h = c(0), v = (0), lty = 2, col = "blue")



