### APUNTES DE CLASE 14/11/2019

####### LUCÍA SAIZ LAPIQUE

######### CLÚSTERING


library(readr)
library(factoextra)
library(cluster)
library(gridExtra)
library(corrplot)

quiebras <- read_delim("C:/Users/Luli/Desktop/TODO/GitHub/lulisaila-Master_Data_Science_CUNEF/Tecnicas de Agrupación y Segmentación/BBDD/quiebras.csv", ";", escape_double = FALSE, trim_ws = TRUE)


# cambiamos la primera columno para quitar la de quiebra o no, porque eso ahora mismo
# nos interesa: el banco es ahora el nombre de la fila 
quiebras <- data.frame(quiebras[,-1], row.names=quiebras$banco)
quiebras

# quitamos los valores perdidos
quiebras = na.omit(quiebras)
head(quiebras)

quiebras_orig <- quiebras
head(quiebras_orig)

q_stats <- data.frame(
  Min = apply(quiebras_orig[,-1], 2, min), # m?nimo
  Med = apply(quiebras_orig[,-1], 2, median), # mediana
  Mean = apply(quiebras_orig[,-1], 2, mean), # media
  SD = apply(quiebras_orig[,-1], 2, sd), # Desviaci?n t?pica
  Max = apply(quiebras_orig[,-1], 2, max) # M?ximo
)

q_stats <- round(q_stats, 2)
head(q_stats)
# activo circulante: no exactamente la solvencia 
# coste de ventas sobre ventas: margenes. Me interesa que sea lo mas bajo posible, ratio menor que 1. 
# trabajar con m?rgenes: eficiente
# Ventas sobre ventas: queremos que este margen sea lo mayor posible. Hay sesgo hacia la derecha, hay algunos valores (pocos) por encima de la mediana
# de cash flow negativo a positivo
# con estos mimbres (medidas de posicion) perdemos perspectiva de las medidas de los bancos, 
# porque cada resumen tiene sus problemas, pero nos sirve para agrupar. 

# Escalar las variables es recomendable cuando vienen en distinta unidad de medida.
# tipificamos y havcemos el analisis con los valores tipificados (solo cuando estan en escala metrica)
# si no tipifico la escala de las variables no sera homogenea
# scale(quiebras) no funciona porque en ese caso, todas las variables tienen q ser numericas
performScaling <- T # on/off para experimentar

if (performScaling) {
  # Loop sobre cada columna
  for (colName in names(quiebras)) {
    # Comprueba si la columna es de datos num?ricos.
    if(class(quiebras[,colName]) == 'integer' | class(quiebras[,colName]) == 'numeric') {
      # escala la columna.
      quiebras[,colName] = scale(quiebras[,colName])
    }
  }
}

head(quiebras, n = 3)
q = data.frame(
  Min = apply(quiebras[,-1], 2, min), # m?nim0
  Med = apply(quiebras[,-1], 2, median), # mediana
  Mean = apply(quiebras[,-1], 2, mean), # media
  SD = apply(quiebras[,-1], 2, sd), # Desviaci?n t?pica
  Max = apply(quiebras[,-1], 2, max) # M?ximo
)

q = round(q, 1)
head(q)
# identificamos como de proximas estan las observaciones entre si, independientemente
# de si quebraron o no. Archivo q sirve para tanto logaritmo supervisado como no supervisado
# cluster me identificara la quiebra de todas formas
# diferencia entre coeficiente de relacion y distancia: 

# calculamos una matriz de distancias:
q.dist <- get_dist(quiebras[,2:10], stand = TRUE, method = "pearson")#solo admite valores num?ricosstr
str(q.dist)

fviz_dist(q.dist, lab_size = 5)
fviz_dist(q.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), lab_size = 5)
# vemos los grupos de bancos distintos, el grafico es sim?trico, y nos intenta indicar
# los posibles grupos en funcoin de la distancia, no es un cluster ni mucho menos
# pero nos da una primera vision de lo que podrian ser los grupos segun lo aproximados
# o alejados que estan los datos. 


dist(quiebras_orig, method = "euclidean")

dist.eucl <- dist(quiebras, method = "euclidean", upper = F)
dist.eucl

rr <- round(as.matrix(dist.eucl)[1:6, 1:6], 1)  # seis primeras observaciones de fila y de columna con dos decimales
rr

quiebras.cor <- cor(t(quiebras[,-1]), method = "pearson") # hacemos una matriz de correlacion: matriz de quiebras traspuesta, ha quebrado o no
# Empleamos quiebras, que es el objeto re-escalado, sin la primera columna.
round(quiebras.cor[1:6, 1:6], 2) # me interesa la distancia o asociacion entre las observaciones

# media del banco 1 es el valor medio de todos los ratios del banco uno
# medimos la asociacion entre los bancos 
# la variable no es el banco, es el ratio. Tengo un conjunto de ratios que he medido en un conjunto de bancos
# sobre cada banco mido la correlacion y a partir de ese coeficiente, su asociacion 

cor <- as.dist(1 - quiebras.cor) # matriz de distancias 
# Las correlaciones negativas tendr?n en la nueva matriz de distancias un valor > 1
round(as.matrix(dist.cor)[1:6, 1:6], 2)

corrplot(as.matrix(dist.eucl), is.corr = FALSE, method = "color", tl.cex = 0.6, tl.col = "blue")
corrplot(as.matrix(dist.eucl), is.corr = FALSE, method = "color", type = "lower",
         diag = F, order = "hclust", tl.cex = 0.6, tl.col = "blue")
# outliers, observaciones que tienen un comportamiento muy distinto a los demas 

plot(hclust(dist.eucl, method = "ward.D2"), cex = 0.7, main = "Dendrograma", ylab = "Anchura",
     xlab = "Análisis cluster aplicando Ward sobre matriz de distancias euclídeas")
# dividimos del todo, 58 bancos hasta 58 unidades o aglomero desde 58 unidades a un unico grupo
# todos los bancos se unen arriba
# con las ramitas vemos qu? bancos estan mas o menos asociados entr ellos. la altura me 
# dice en que etapa ha ocurrido y el ancho como de unidas estan. Cuanto mayor la etapa, mas tardia
# la union. 

heatmap(as.matrix(dist.eucl), symm = TRUE, distfun = function(x) as.dist(x))

quiebras.eclust = eclust(quiebras[,-1], FUNcluster = "kmeans", stand = TRUE,
                         hc_metric = "euclidean", nstart = 25)
quiebras.eclust = eclust(quiebras[,-1], FUNcluster = "kmeans", stand = TRUE,
                         hc_metric = "euclidean", nstart = 25, k = 4)
# cluster automatico:

fviz_silhouette(quiebras.eclust)
# representacion de la bondad de la asignacion de cada una de las variables al clluster

# el codigo postal se debe quitar, porque es discriminante
