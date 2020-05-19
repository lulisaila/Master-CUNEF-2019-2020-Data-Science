############ PRACTICA 6
#########SERIES TEMPORALES: COLGATE VS CREST
######LUCIA SAIZ LAPIQUE



library(forecast)
library(xts)
library(ggplot2)
library(readxl)
library(TSA)
library(zoo)
library(stats)
library(astsa)
library(dynlm)
library(skimr)

#read data from CSV file
pasta <- read_excel("data.xlsx")

summary(pasta)
skim(pasta) 

#Create a XTS object
pasta$Date <- as.Date(paste(pasta$Year, pasta$Week, 1, sep = "-"), "%Y-%U-%u")
xColgate <- ts(data = pasta$Colgate, start = 1958, end =  1963, frequency = 52)
xCrest <- ts(data = pasta$Crest, start = 1958, end =  1963, frequency = 52)

#Transform to zoo data (forecast package)
zColgate <- as.zoo(xColgate)
zCrest <- as.zoo(xCrest)

##Plot Serie
autoplot(zColgate) + ylab("Colgate") + ggtitle("Colgate Cuota de Mercado") + xlab("Semanas")
autoplot(zCrest) + ylab("Crest") + ggtitle("Crest Cuota de Mercado") + xlab("Semanas")

df_new_colg <- data.frame(value = as.vector(zColgate),
                     time = time(zColgate))
ggplot(df_new_colg) +
  geom_point(aes(x = time, y = value)) +
  geom_line(aes(x = time, y = value)) +
  ylab("Colgate") +
  ggtitle("Cuota de mercado de Colgate") +
  xlab("Semanas")

df_new_crest <- data.frame(value = as.vector(zCrest),
                     time = time(zCrest))
ggplot(df_new_crest) + geom_point(aes(x = time, y = value)) + geom_line(aes(x = time, y = value)) +
  ylab("Colgate") + ggtitle("Cuota de mercado de Crest") + xlab("Years")

#Difference
ggtsdisplay(zColgate) # serie sin diferencias, claramente no estacionaria
ggtsdisplay(zCrest)

ggtsdisplay(diff(zColgate)) # serie de una diferencia, tasa trimestral, no 
# estacionaria porque tiene un componente estacional muy fuerte
ggtsdisplay(diff(zCrest))

ggtsdisplay(diff(zColgate, 4)) # tasa de variacion anual, esta si que parece estacionaria
ggtsdisplay(diff(diff(zColgate, 4), 1))

ggtsdisplay(diff(zCrest, 4))
ggtsdisplay(diff(diff(zCrest, 4), 1))

#Select number of observation to compare forecast
cOmit = 16

#Data Size
nObs_colg = length(zColgate)
nObs_crest = length(zCrest)

#sub_sample
oColgate <- window(zColgate, start = index(zColgate[1]), end = index(zColgate[nObs_colg - cOmit]))
oCrest <- window(zCrest, start = index(zCrest[1]), end = index(zCrest[nObs_crest - cOmit]))
# original, training

#out sample (real data to forecast performance)
pColgate <- window(zColgate, start = index(zColgate[nObs_colg - cOmit + 1]), end = 
                     index(zColgate[nObs_colg]))
pCrest <- window(zCrest, start = index(zCrest[nObs_crest - cOmit + 1]), end = index(zCrest[nObs_crest]))
# p prediccion, testing

#ARIMA MODEL
fit1_colg = auto.arima(oColgate, lambda = "auto") # el logaritmo es que lambda sea 0, por defecto 
# de forma automatica me selecciona el arima
fit1_crest = auto.arima(oCrest, lambda = "auto")
summary(fit1_colg)
summary(fit1_crest)
# ARIMA(1,0,0)(0,1,1)[4] with drift -- drift me dice que tiene constante. 0 regular,
# una diferencia estacional, como es el logaritmo me dice que esta calculando el 
# neperiano de las ventas menos el neperiano de las ventas de t-4, tasa de 
# variacion logaritmica anual depende autorregresivo de lo que paso en el periodo 
# anterior, y del error de hace 4 periodos. Si quieres predecir la venta de coca 
# cola tienes q calcular la tasa  de variacion anual y tener ren cuenta su error 
# de ahora y el de hace 4 años. 
# importante ahora: analizar los errores para ver si son ruido blanco

#residual analysis
checkresiduals(fit1_colg)
checkresiduals(fit1_crest)

fcolgate = forecast(fit1_colg)
fcrest = forecast(fit1_crest)

ggplot(df_new_colg) + geom_point(aes(x = time, y = value)) + geom_line(aes(x = time, y = value)) +
  geom_forecast(fcolgate, alpha = 0.4) + ggtitle("ARIMA: Prediccion Colgate")
ggplot(df_new_crest) + geom_point(aes(x = time, y = value)) + geom_line(aes(x = time, y = value)) +
  geom_forecast(fcrest, alpha = 0.4) + ggtitle("ARIMA: Prediccion Crest")


fcolgate
fcrest

# Pueden ser aditivos o innovativos. Si no sigue el modelo, es innovativo y me aparece en 
# el error. Nos dicen de forma automatica si son aditivos o innovativos. 

arimax_Colgate = arimax(log(oColgate), order = c(0,1,1),
              xtransf = data.frame(I911 = 1 * (seq(oColgate) == 135), # a manubrio
                                 I911 = 1 * (seq(oColgate) == 135)),
              transfer = list(c(0,0), c(1,0)),
              method = 'ML')

detectAO(arimax_Colgate) # no hay aditivos
detectIO(arimax_Colgate) # si que hay innovativos 

arimax_Colgate2 = arimax(log(oColgate), order = c(0, 1, 1),
                        xtransf = data.frame(I911 = 1 * (seq(oColgate) == 135), # a manubrio
                                             I911 = 1 * (seq(oColgate) == 135)),
                        transfer = list(c(0,0), c(1,0)),
                        io = c(102),
                        method = 'ML')
# io porque es innovativo
# me estima el efecto que tiene el io y me quita el outlier

arimax_Crest = arimax(log(oCrest), order = c(0, 1, 1),
                        xtransf = data.frame(I911 = 1 * (seq(oCrest) == 135), # a manubrio
                                             I911 = 1 * (seq(oCrest) == 135)),
                        transfer = list(c(0,0), c(1,0)),
                        method = 'ML')

detectAO(arimax_Crest) # no hay aditivos
detectIO(arimax_Crest) # si que hay innovativos 

arimax_Crest2 = arimax(log(oCrest), order = c(0, 1, 1),
                         xtransf = data.frame(I911 = 1 * (seq(oCrest) == 135), # a manubrio
                                              I911 = 1 * (seq(oCrest) == 135)),
                         transfer = list(c(0, 0), c(1, 0)),
                         io = c(99),
                         method = 'ML')

plot(arimax_Colgate2$coef, type = "h")
plot(arimax_Crest2$coef, type = "h")

arimax_Colgate2 = arimax(log(oColgate), order = c(0, 0, 1),
                         xtransf = data.frame(I911 = 1 * (seq(oColgate) == 135), # a manubrio
                                              I911 = 1 * (seq(oColgate) == 135)),
                         transfer = list(c(0, 0), c(1, 0)),
                         io = c(99),
                         method = 'ML')