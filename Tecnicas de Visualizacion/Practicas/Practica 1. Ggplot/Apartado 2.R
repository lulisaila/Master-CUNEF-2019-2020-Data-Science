# 2) en un fichero llamado apartado2.R una gráfica con todas las variables con
# lineas de distintos colores a lo largo del tiempo (tiempo = eje X)

library(ggplot2)
eco <- economics
ecol <- economics_long


#ECONOMICS
ggplot(eco, aes(x = date)) +  # para cada variable, tenemos que crear una linea, a la que
# le adjudicamos un color distinto para que se vean bien. En la leyenda, apareceran los 
# nombres de las variables junto al color respectivo
  geom_line(aes(y = pce, color = "pce")) +
  geom_line(aes(y = pop, color = "pop")) +
  geom_line(aes(y = psavert, color = "psavert")) +
  geom_line(aes(y = uempmed, color = "uempmed")) +
  geom_line(aes(y = unemploy, color = "unemploy"))


#ECONOMICS_LONG
ggplot(ecol, aes(x = date, y = value01)) +  # En este caso, usamos solo la variable 
# value01 porque es la variable cuyos datos estan estandarizados. Representamos la grafica
# por colores, al igual que el anterior, asignandole a cada variable un color. 
   geom_line(aes(color = variable))
   