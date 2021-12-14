
P             <- matrix(data = c(0,1,1,0), nrow = 2, ncol = 2, byrow = TRUE)
num_pasos     <- 10000
cadena_markov <- vector(mode = "numeric", length = num_pasos)
estado_actual <- 1
for( k in seq(num_pasos) ){
  estado_actual <- sample(x = c(1,2), size = 1, prob = P[estado_actual,] )
  cadena_markov[k] <- estado_actual
}
prop.table(table(cadena_markov))

matrixcalc::matrix.power(x = P, k = 10000)
matrixcalc::matrix.power(x = P, k = 10001)

# Definición de distribución estacionaria
# pi = pi * P
matrix(data = c(1/2,1/2), nrow = 1, ncol = 2) %*% matrixcalc::matrix.power(x = P, k = 10001)
matrix(data = c(1/2,1/2), nrow = 1, ncol = 2) %*% matrixcalc::matrix.power(x = P, k = 10002)




