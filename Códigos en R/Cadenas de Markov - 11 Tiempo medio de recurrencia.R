####################################################################################################
# TIEMPO MEDIO DE RECURRENCIA - CASO GENERAL PARA CADENA DE 2 ESTADOS
####################################################################################################

# FIJAMOS LOS VALORES DE 'a' Y 'b'
a <- runif(n = 1) #1/9
b <- runif(n = 1) #3/7

# Generamos nuestra matriz de 2 estados
P <- matrix(data = c(1-a, a, b, 1-b), nrow = 2, ncol = 2, byrow = TRUE)

# DEJEMOS CORRER LA CADENA CIERTO TIEMPO
x0            <- 1      # Estado Inicial
num_pasos     <- 10000  # Numero de pasos que se dejara correr la cadena de Markov
cadena_markov <- vector(mode = "numeric", length = num_pasos) # Vector con los estados de la cadena
x_actual      <- x0
for( k in seq(num_pasos) ){
  # Generamos el siguiente estado de la cadena de acuerdo al actual
  x_siguiente      <- sample( x = c(1,2), size = 1, replace = FALSE, prob = P[x_actual,] )
  cadena_markov[k] <- x_siguiente
  x_actual         <- x_siguiente
}
# Finalmente, a la cadena le agregamos el estado inicial
cadena_markov <- c(x0, cadena_markov)

# Calculemos el tiempo medio de recurrencia del estado 1
tiempos_llegada_1 <- which(cadena_markov==1)
tiempos_regreso_1 <- diff(tiempos_llegada_1)
mu_1              <- mean(tiempos_regreso_1)
mu_1      # Tiempo medio de recurrencia práctico
(a+b)/b   # Tiempo medio de recurrencia teórico


# Calculemos el tiempo medio de recurrencia del estado 2
tiempos_llegada_2 <- which(cadena_markov==2)
tiempos_regreso_2 <- diff(tiempos_llegada_2)
mu_2              <- mean(tiempos_regreso_2)
mu_2      # Tiempo medio de recurrencia pr?ctico
(a+b)/a   # Tiempo medio de recurrencia te?rico

# Notemos que la siguiente expresi?n nos regresa un valor cercano a 1
1/mu_1 + 1/mu_2 = 1

1/mean(cadena_markov==1) == mu_1
1/mean(cadena_markov==2) == mu_2

mean(cadena_markov==1) == 1/mu_1
mean(cadena_markov==2) == 1/mu_2

diag(matrixcalc::matrix.power(x = P, k = 100))

# la proporcion del tiempo que pasa la cadena en un estado es inversamente proporcional
# al tiempo medio de recurrencia

# Esto es porque 1/mu_1 = b/(a+b) y 1/mu_2 = a/(a+b)
# los cuales son las distribuciones estacionarias halladas en la clase anterior

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################