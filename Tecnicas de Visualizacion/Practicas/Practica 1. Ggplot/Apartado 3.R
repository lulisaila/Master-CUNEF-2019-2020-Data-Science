# 3) en un fichero llamado apartado3.R haz un scatterplot de waitings (eje Y) 
# con eruptions (eje X)

library(ggplot2)
faith <- faithful

ggplot(faith, (aes(x = eruptions, y = waiting))) +  # los scatterplot se crean con la 
# funcion geom_point derivada de un ggplot, asi que usando las variables x = eruptions 
# e y = waitings de la base de datos que nos pide el ejercicio utilizar, representamos 
# el grafico. 
  geom_point(color = "turquoise") # le añadimos color a la grafica con la funcion color
# y poniendole entre commillas el nombre (en ingles) del color que queremos usar
