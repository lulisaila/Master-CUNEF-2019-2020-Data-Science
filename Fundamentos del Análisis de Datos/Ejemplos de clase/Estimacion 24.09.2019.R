#####################  INFERENCIA PARAMETRICA #######################

###########################  ESTIMACION  ###########################


library(MASS)



set.seed(2)   # Fijamos la semilla

x <- rnorm(250, mean = 1, sd = 0.45)     


# Ajuste maxima verosimilitud de distribuciones univariables

fit <- fitdistr(x, densfun = "normal") 
fit # nos da tambien, ademas de la media y varianza muestral, la dispersion que tiene 

hist(x, pch = 20, breaks = 25, prob = TRUE, main = "HISTOGRAMA") 
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col = "pink", lwd = 2, add = T) 
# queremos que nos ajuste los valores de probabilidad de una normal que yo he estimado previamente. 



#Estimacion por el metodo de los momentos 
fitMME <- fitdistr(x, "normal", method = "mme") # tambien nos va a dar los dos parametros
fitMME
curve(dnorm(x, fitMME$estimate[1], fitMME$estimate[2]), col = "red", lwd = 2, add = T)



#Estimacion maxima bondad del ajuste
#Diversos estudios destacan la mejor estimaci?n param?trica 
#Hay unos valores empiricos reales en miuestra y otros ajustados por unos parametros; 
#esto lo que hace es inimizar la diferencia entre esos valroes 

fitMGE <- fitdistr(x, "normal", method = "mge")
fitMGE


