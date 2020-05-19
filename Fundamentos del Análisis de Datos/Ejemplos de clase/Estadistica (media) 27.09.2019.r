#################### FUNDAMENTOS DE ESTADISTICA ############################

########################### INFERENCIA  ###################################




#install.packages("ggpubr")

library(ggpubr)




datos1 <- read.csv("./index.csv")


datos1

summary(datos1$Index)



#Si n<30 ---- verificamos la normalidad


shapiro.test(datos1$Index)

# si p-valor >0.05 asumimos normalidad




#Visualización de la normalidad de los datos usando qqplots

#(cuantiles de la muestra y la normal)

  ggqqplot(datos1, x="Index",ylab = "Calificaciones", xlab = "Teóricas",
         ggtheme = theme_minimal())


  
  

#Si los datos no se distribuyen normalmente se realizará el

  #test de Wilcoxon. 



##################### Test

############Sobre la media de la población
  

  # hO:MU=20


resultado1 <- t.test(datos1$Index, mu = 20)

# Obtenemos resultados

resultado1 # no se puede avceptar esa hipotesis nula con estos datos porque el p value es demasiado 
# pequeño. Hipotesis se queda fuera del intervalo; no se puede aceptar esa hipotesis.

#Al obtener un p-valor < 0.05, el valor medio de la muestra es significativamente diferente a 20.



#Alternativa inferior

resultado2 <- t.test(datos1$Index, mu = 20,
       alternative = "less")  #decimos cual es la alternativa; que es menor. 
#Contraste de dos colas donde contrastamos q la hipotesis nula dice que la meida vale 20 y la 
#alternativa que la media es menor de 20

resultado2 #p valor muy pequeño - no se puede aceptar la hipotesis. Nuevo contraste con un valor inferior 


#Alternativa mayor

resultado3 <- t.test(datos1$Index, mu = 20,
       alternative = "greater") 

resultado3



# Devuelve el p-valor
resultado1$p.value

# Devuelve el valor medio
resultado2$estimate

# Intervalo de confianza
resultado3$conf.int

