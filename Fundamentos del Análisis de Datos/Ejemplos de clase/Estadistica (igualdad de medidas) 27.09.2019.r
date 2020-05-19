#################### FUNDAMENTOS DE ESTADISTICA ############################

########################### INFERENCIA  ###################################

# Contraste igualdad medias 


library("dplyr")
library("ggpubr")
library("PairedData")



# vamos a estudiar la variable "peso" al inicio y final de un periodo:


datos3 <- read.csv("./pesos.csv")


datos3

summary(datos3$Peso)




#Ofrece el resumen estadístico para los distintos grupos


group_by(datos3, Grupo) %>%
        summarise(
                count = n(),
                mean = mean(Peso, na.rm = TRUE),
                sd = sd(Peso, na.rm = TRUE))
        



#VISUALIZACION DE DATOS

#install.packages("devtools") #No requerido 


# Grafico de caja y bigotes

# Grafico por peso y grupo


ggboxplot(datos3, x = "Grupo", y= "Peso", 
          color = "Grupo", palette = c("#00AFBB", "#E7B800"),
          order = c("Comienzo", "Final"),
          ylab = "Peso", xlab = "Grupos")





# Otros gráficos


# Subgrupo de pesos inicio periodo
Comienzo <- subset(datos3,  Grupo == "Comienzo", Peso,
                 drop = TRUE)
# Subgrupo de pesos final periodo
Final <- subset(datos3,  Grupo == "Final", Peso,
                drop = TRUE)

# Grafico de grupos

pd <- paired(Comienzo, Final)
plot(pd, type = "profile") + theme_bw()




# Verificar la normalidad con Test Shapiro-Wilk

d <- with(datos3, 
          Peso[Grupo == "Comienzo"] - Peso[Grupo == "Final"])

shapiro.test(d) 




# Contrastes (Peso ~ (alt 126) Grupo)



res <- t.test(Peso ~ Grupo, data = datos3, paired = TRUE) #paired=TRUE; emparejados,  que tenemos dos 
#muestras. La misma funcion, pero utilizada de otra forma. 
res
#en este test, decimos que la hipotesis nula representa que  no hay diferencias entre las medias 
#(que son iguales). P valor muy proximo al 5%,  


#La media es menor al comienzo

t.test(Peso ~ Grupo, data = datos3, paired = TRUE,
       alternative = "less")


#La media es mayor al comienzo
t.test(Peso ~ Grupo, data = datos3, paired = TRUE,
       alternative = "greater")

# p-valor
res$p.value

# Media de las diferencias
res$estimate

# Intervalo de confianza
res$conf.int

