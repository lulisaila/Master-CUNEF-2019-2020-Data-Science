#-----------------------------------------------
# MASTER DATA SCIENCE Y FINANZAS #
# FUNDAMENTOS DEL ANALISIS DE DATOS: PRACTICA 4 #
# LUCIA SAIZ LAPIQUE #

#Apartado a

Prob_parking <- function(coches, vacios, espera) { # creamos una nueva funcion en la que introduzcamos 
  #las variables coche, vacio(espacios en el parking vacios) y espera (espacios de espera)
  
  lambda <- 6  # introducimos los valores de lambda y mu que nos proporciona el ejercicio y añadimos un
  #contador para los bucles que queremos utilizar
  
  mu <- 2
  cont <- 0
  
  for (i in (1:(vacios + espera))) { # a partir de aqui, introducimos una serie de bucles que contengan 
    # condiciones para satisfacer a la formula que calcula la pribabilidad cero y la probabilidad de n
    if (i <= vacios) { # si el numero de coches es menor al numero de huecos vacios que hay
      cont <- cont + (((lambda/mu)^i)/factorial(i))  # la formula es Î»1*P1 = mu0*P0, y asi sucesivamente 
      # segun van creciendo las n, con lo cual la formula escrita seria Pn = P0*Î»^n, y lo representamos 
    }
    
    else{
       cont <- cont + (((lambda/mu)^i)/(factorial(vacios) * vacios^(i - vacios))) # en caso de que estemos 
       #hablando de los huecos de espera (hay mayor numero de coches que sitios vacios), debemos tener en 
       #cuenta el valor de esos
    }
    
  }
  P0 <- 1/(1 + cont)   # debemos calcular P0 para sacar la probabilidad de n final 
  
  if(coches <= vacios) {
    return((((lambda/mu) ^ coches)/factorial(coches))*P0)
  }
  
  else {
    return(((lambda/mu)^coches)/(factorial(vacios) * vacios ^ (coches - vacios)) * P0)
  }
}

Prob_parking(6,5,3) # como ejemplo, vemos la probabilidad de que haya "n" coches en el sistema, con el 
# ejemplo de n = 6, y los huecos libres y de espera siendo 5 y 3 respectivamente
# el resultado de esa probabilidad es 0.05846574, es decir que hay un 5.85% de probabilidad de que haya 
# 6 coches en total en el parking

Probabilidad <- for(i in 0:8) { # finalmente, una vez confirmamos que la funcion nos da el resultado 
  # esperado, le pedimos que nos imprima una lista con todas las probabilidades de cada numero de coches 
  # que pueda haber en el parking a la vez. La suma de todos ellos deberia dar 1
  print(Prob_parking(i,5,3))
}
#[1] 0.04811995 para P0
#[1] 0.1443599 para P1
#[1] 0.2165398 para P2
#[1] 0.2165398 para P3
#[1] 0.1624048 para P4
#[1] 0.09744291 para P5
#[1] 0.05846574 para P6
#[1] 0.03507945 para P7
#[1] 0.02104767 para P8

# Para los demas apartados, aplicamos las respectivas formulas a las probabilidades que sacamos del 
# apartado anterior


# Apartado B:
# Tasa perdida: Î » perdido = Î » p8 

Tefectiva <- 6 * 0.02104767 # = 0.126286

# lambda efectiva sera el valor de lambda original menos la tasa perdida (no lo pide el ejercicio, 
# pero luego lo vamos a usar):

Xefectiva <- 6 - 0.126286  # Î » efectiva = 5.873714


# Apartado C:

# Media de coches en el estacionamiento  = 0p0 + 1p1 + â€¦ + 8p8
Promedio <- 0.04811995*0 + 0.1443599*1 + 0.2165398*2 + 0.2165398*3 + 0.1624048*4 + 
  0.09744291*5 + 0.05846574*6 +  0.03507945*7 + 0.02104767*8 # = 3.128625

# Apartado D:

# cantidad de tiempo hasta que hay un hueco libre = Media coches/Î»efectiva - 1/mu
Tiempo <- Promedio/Xefectiva - 1/2 # Tiempo de espera = 0.03264844


# Apartado E:

# Media de huecos ocupados en ese tiempo: Î»efectiva/mu
MediaEsp <- Xefectiva/2 # Media espacios = 2.936857

# Todos los resultados estan explicados en el PDF adjunto 