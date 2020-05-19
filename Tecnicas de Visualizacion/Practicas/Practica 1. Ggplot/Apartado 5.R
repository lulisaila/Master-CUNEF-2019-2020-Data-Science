# 5) en apartado5.R evita el overplotting usando jittering

library(ggplot2)
faith <- faithful


ggplot(faith, (aes(x = eruptions, y = waiting, alpha = 0.3, position = "jitter"))) +
  geom_point()  # MAnteniendo el alpha como lo teniamos en el ejercicio anterior, 
# introducimos la funcion jitter, que lo que hace es, de una forma mas eficiente que
# usando alpha, evita el overplotting separando un poco los puntos entre si. 

    