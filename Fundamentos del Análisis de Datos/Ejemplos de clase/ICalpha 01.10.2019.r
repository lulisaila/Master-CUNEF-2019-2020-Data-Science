#################### FUNDAMENTOS DE ESTADISTICA ############################

########################### FUNCION ICALPHA  ###################################

ICalpha <- function(ModeloA, ModeloB, alfa = 0.05)
{ 
  n <- length(ModeloA)
  diferencias <- ModeloA - ModeloB
  mediad <- mean(diferencias)
  #mediad2 <- mean(diferencias^2)
  
  s <- sqrt(var(diferencias))
  #s <- sqrt(mediad2-mediad^2)
  
  valort <- qt(alfa/2,n - 1,lower.tail = F)
  valor <- valort*s/sqrt(n)
  
  cotaInf <- mediad - valor
  cotaSup <- mediad + valor
  
  df <- data.frame(cotaInf, cotaSup)
  
  return(df)
}