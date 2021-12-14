########################################################################################
# SIMULACIONES PARA ENCONTRAR LA DISTRIBUCIÓN ESTACIONARIA DE MATRICES
# QUE SON DOBLEMENTE ESTOCASTICAS
########################################################################################

# Matriz de 2x2
P <- matrix(data = c(1/4,3/4,
                     3/4,1/4), nrow = 2, ncol = 2, byrow = TRUE)

# Matriz de 4x4 
P <- matrix(data = c(3,4,1,2,
                     3,4,3,0,
                     3,1,2,4,
                     1,1,4,4), nrow = 4, ncol = 4, byrow = TRUE) / 10

# Matriz de 4x4 que al mismo tiempo es simétrica
P <- matrix(data = c(1,2,3,4,
                     2,4,3,1,
                     3,3,2,2,
                     4,1,2,3), nrow = 4, ncol = 4, byrow = TRUE) / 10

# Matriz de 7x7 
P <- matrix(data = c(7,2,5,4,1,3,3,
                     6,4,3,3,1,4,4,
                     5,4,3,5,2,3,3,
                     1,5,1,3,7,5,3,
                     2,3,1,6,3,6,4,
                     1,2,5,2,9,1,5,
                     3,5,7,2,2,3,3), nrow = 7, ncol = 7, byrow = TRUE) / 25

# all( t(P) == P ) # Para validar que la matriz sea simétrica
# colSums(P)       # Para validar que la matriz sea estocástica por filas
# rowSums(P)       # Para validar que la matriz sea estocástica por columnas

set.seed(98368127)
espacio_estados <- seq(nrow(P))
num_pasos       <- 500000
cadena_markov   <- vector(mode = "numeric", length = num_pasos)
estado_actual   <- 1
for( k in seq(num_pasos) ){
  estado_actual <- sample(x = espacio_estados, size = 1, prob = P[estado_actual,] )
  cadena_markov[k] <- estado_actual
}
prop.table(table(cadena_markov))

matrixcalc::matrix.power(x = P, k = 10000) # Distr. Limite par
matrixcalc::matrix.power(x = P, k = 10001) # Distr. Limite impar

########################################################################################
# FIN DEL ARCHIVO
########################################################################################