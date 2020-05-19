###########TAREA 2 FUNDAMENTOS ANALISIS DATOS##############

######################LA CESTA: LUCIA SAIZ########################

clientes <- read.csv("C:/Users/Luli/Desktop/Master Primer Cuatri/Fundamentos/LA CESTA.csv")
clientes

media <- mean(clientes$x) # queremos la media de todos los datos que estan en la columna de la variable x
dt <- sd(clientes$x)
n <- 600 

#a. Que en un minutos cualquiera no acceda ningun cliente.
dpois(x = 0, lambda = 0.78)


#b. Que en un minuto accedan entre 2 y 5 clientes.
sum(dpois(x = 2:5, lambda = 0.78))
ppois(2:5, 0.78)
?ppois

#Que en un minuto cualquiera accedan 3 clientes
dpois(x=3, lambda = 0.78)


#c. Que en 5 minutos accedan mas de 10 clientes
mediacinco <- 0.78*5
mediacinco

ppois(q = 10, lambda = 3.9, lower.tail = FALSE)

#Adicionalmente, sera necesario conocer tambien entre que valores se podria 
#encontrar el numero medio de clientes accediendo a la cola unica, con un nivel 
#de confianza del 95%
alpha <- 0.05

cuantil <- qnorm(1 - alpha/2)

lim_inf <- media - cuantil * sqrt(dt^2) / sqrt(n)
lim_inf

lim_sup <- media + cuantil * sqrt(dt^2) / sqrt(n)
lim_sup

#Con las muestras de otra empresa sacamos unos datos
muestra2 <- 500
media2 <- 0.69
dt2 <- 0.96

empresa2 <- as.data.frame(rnorm(500, mean = 0.69, sd = 0.96))
comparativa <- t.test(empresa2, clientes$x)
comparativa
