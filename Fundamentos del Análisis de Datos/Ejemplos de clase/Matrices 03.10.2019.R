##################FUNDAMENTOS DE ALGEBRA MATRICIAL##################



#A, B, C, D,... seran nombres de  matrices
#x, y, z, .... seran nombres de vectores (fila/columna)

#Vectores

x <- c(1,2,3,4)

length(x) #Devuelve la dimension (No elementos)

x  #Devuelve un vector fila




#Matrices
A <- matrix(c(1,2,3,4,5,6), nrow = 2, ncol = 3)
B <- matrix(c(1,2,3,4,5,6), 2, 3, byrow = T)  # byrow hace que te haga una matriz con los datos 
# intorducidos colocandolos por filas; si no se pone nada, lo pone por columnas
A
B

dim(A) #Devuelve la dimension, que es el numero de filas por el de columnas
length(A) #Devuelve el numero de elementos que hay, su longitud


#EXTRACCIONES

A[1,2]  #Devuelve el elemento de la posicion seleccionada
B[2,2]
A[1,]  #Devuelve la primera fila 
B[,2]  #Devuelve la segunda columna



#CALCULOS

A + B # se pueden sumar las matrices

A - B # se pppueden restar las matrices

A*2

t(A) #Matriz traspuesta
t(B)

t(x)  #Matrix de x (dimension 1*4). 

dim(t(B)) #Devuelve la dimension de la traspuesta

dim(t(x))
matrix(x)  #Matrix de x (dimension 4*1). 
dim(matrix(x))


# MULTIPLICACION

#Multiplica elemento por elemento  - OJO -

A*B  # Multiplica cada elemento de A por el elemento que esta en su misma posicion de la B; no es 
# correcto, asi es como se suman matrices pero multiplicar matrices se hace de otra forma


#Multiplicacion de matrices

Q <- matrix(c(1,2,3,4,5,6), 2, 3, byrow = T) 
W <- matrix(c(7,8,9,10,11,12), 3, 2, byrow = T) # creamos dos nuevos matrices

Q
W

Q %*% W # esta es la forma correcta de multiplicar matrices
W %*% Q



A %*% B # ?  # tienen que tener dimensiones opuestas para poder multiplicarlas, por eso da error
B %*% A # ?


#Uniendo matrices

A
C <- matrix(c(1,2,3,4,5,6), 2, 3, byrow = F)
C


cbind(A,C)  #Junta las columnas
rbind(A,C)  #Junta las filas

t(rbind(t(A), t(C)))
t(cbind(t(A), t(C)))


#Diagonales y trazas


D <- matrix(c(1,2,3,4,5,6,7,8,9), 3, 3, byrow = T)
D

diag(D)  #Devuelve la diagonal de la matriz
sum(diag(D))  #Devuelve la traza


diag(c(1,2,3))  #Matriz diagonal

diag(diag(D))   #Devuelve la matriz diagonal con los elementos de la diagonal D





#Determinantes

D <- matrix(c(1,-2,2,2,0,1,1,1,-2), 3, 3, byrow = T)
D
det(D)


#Inversas

#MATRIZ NO SINGULAR

E <- matrix(c(2,3,3,4), 2, 2, byrow = T)
E
solve(E) #Calculo para matrices no singulares



#DIAGONALIZACION DE MATRICES

G <- matrix(c(1,4,9,1), 2, 2, byrow = T)
G
eigen(G) #Proporciona autovalores y autovectores asociados





# APLICANCION EN PROCESOS ESTOCASTICOS

PE <- matrix(c(1,2,1,-1), 2, 2, byrow = T)
PE
solve(PE) 



#Matriz de transici?n

PE <- matrix(c(0.5, 0.5, 0.25, 0.75), 2, 2, byrow = T)
PE

a <- eigen(PE) #autovalores y autovectores asociados
a


#MATRIZ H DE LOS AUTOVECTOREs
H <- matrix(c(-0.7071068,-0.8944272,-0.7071068,0.4472136), 2, 2, byrow = F)
H


# O tmbien se pueden extraer de a

autovec <- a$vectors 
H <- matrix(autovec,2,2)
H

#calculo de la inversa
H; solve(H)
HI <- matrix(c(-0.4714045,-0.942809,-0.7453560,0.745356), 2, 2, byrow=T)
HI




# Matrix J de autovalores

J <- matrix(c(1,0,0,0.25), 2, 2, byrow = T)
J
# Calculo de Pn=HJ^nH^(-1)  donde en J^n hacemos n=100

H %*% J^100 %*% HI

#El proceso se estabiliza en DP=1/3  y en RV=2/3 

