    #################### FUNDAMENTOS DE ESTADISTICA ############################

########################### INFERENCIA  ###################################

    #CONTRASTE SOBRE LA PROPORCION



# Contraste sobre proporcion con n pequeño

binom.test(x, n, p = 0.5, alternative = "two.sided")


# Contraste sobre proporción con n suficientemente grande: TCL

prop.test(x, n, p = NULL, alternative = "two.sided",correct = TRUE)


#"CORRECT" - Corrección  de  continuidad  o  de  Yates:   cuando  aproximamos  una  
#distribución binomial mediante una normal, estamos convirtiendo una variable X discreta 
#en una continua.  



test1 <- prop.test(x = 120, n = 200, p = 0.5, correct = TRUE)  # tenemos una binomial y calculamos p # P = 120/200 -- 0.6
test1 # puede 0.6 pertenecer a la poblacion de los datos que nos han dado o no? 
# se rechaza la hipotesis nula porque el p valor es muy bajo, y ademas,  el IC nos muestra que 0.5 se queda fuera del intervalo.

#ofrece el p-valor
test1$p.value

#ofrece el estimador puntual
test1$estimate

#proporciona el intervalo de confianza
test1$conf.int




#cuando la alternativa es <0.5
test2 <- prop.test(x = 120, n = 200, p = 0.5, correct = FALSE,
          alternative = "less")
test2




#cuando la alternativa es >0.5
test3 <- prop.test(x = 120, n = 200, p = 0.5, correct = FALSE,
          alternative = "greater")
test3

