# 1) en un fichero llamado apartado1.R una gráfica con una línea temporal 
# representando la variable "unemploy" (tiempo = eje X)

library(ggplot2)
eco <- economics # guardamos la base de datos en un nuevo objeto para que sea mas facil de usar
economics_long

ggplot(eco ,aes(x = date, y = unemploy)) + geom_line() # creamos un grafico de linea normal
# usando en el eje de las x a la variable fecha y en el de las y unemploy y lo representamos.
# La base de datos economics_long no tiene grafico porque no contiene la variable
# unemploy que nos pide representar el ejercicio. 

