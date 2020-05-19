#################### FUNDAMENTOS DE ESTADISTICA ############################

########################### CADENAS DE MARKOV  ###################################


library(markovchain)

estados <- c("soleado", "nublado", "lluvioso")
byRow <- TRUE

mt <- matrix(data = c(0.6, 0.3, 0.1, 0.4, 0.4, 0.2, 0.25, 0.45, 0.3), byrow = byRow, 
             nrow = 3, dimnames = list(estados, estados))
#La funcion matrix nos hace la matriz estocastica por filas, si la hiciesemos por columnas, la matriz 
#saldria traspuesta
mt

cmclima <- new("markovchain", states = estados, byrow = byRow,
               transitionMatrix = mt, name = "clima")
cmclima
plot(cmclima)

sinicial <- c(0, 1, 0)

dosdespues <- sinicial * (cmclima * cmclima)
sietedespues <- sinicial  * (cmclima ^ 7)

dosdespues
sietedespues

  
  
  
#Ejercicio 1
nombres <- c("A", "B", "C")
byRow <- TRUE

mt <- matrix(data = c(630/700, 28/700, 42/700, 105/750, 570/750, 75/750, 55/550, 44/550, 451/550), 
             byrow = byRow, nrow = 3, dimnames = list(nombres, nombres))
mt

compra <- new("markovchain", states = nombres, byrow = byRow,
               transitionMatrix = mt, name = "compra")
compra
plot(compra)

vo <- c(700/2000, 750/2000, 550/2000) # situacion inicial 
vo

marzo <- vo * compra  # Vo * P
mayo <- vo * compra ^ 3 # cada mes eleva uno mas, como desde febrero hasta mayo hay una diferencia de 
# tres meses, elevamos al cubo

marzo
mayo

# es un metodo evolutivo, ya que el vector que recoge la informacion mes a mes siempre va cambiando.