# 4) en apartado4.R evita el overplotting del anterior ejercicio usando
# transparencia

library(ggplot2)
faith <- faithful


ggplot(faith, (aes(x = eruptions, y = waiting))) + # igual que el ejercicio anterior
  geom_point(alpha = 0.3) # para evitar el overplotting, existe la funcion alpha, que 
# segun el grado que se le adjudique, muestra los puntos de la grafica con mayor o 
# menor transparencia. En este caso he elegido el 0.3, porque cuanto menor el valor
# de alpha, mayos la transaprencia, pero queria que se siguieran interpretando 
# los datos. 

