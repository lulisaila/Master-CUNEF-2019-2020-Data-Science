# 6) en el apartado6.R solapa el jittering y la estimación de densidad


library(ggplot2)
faith <- faithful


ggplot(faith, (aes(x = eruptions, y = waiting, position = "jitter"))) +
  geom_point(color = "pink") +
  geom_density_2d(color = "purple")  # por ultimo, introducimos la funcion de densidad,
# recoge los datos y tambien evita le overplotting como aparece en el grafico

