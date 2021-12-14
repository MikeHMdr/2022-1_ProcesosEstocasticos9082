####################################################################################################
# Ejercicio de simulación para la tarea 4 opcional ejercicio 3
####################################################################################################

#######################################################
# Parámetros iniciales
c <- 3
d <- 5

# {1, 2, 3, ..., c, c+1, c+2, c+3, ..., c+d}
# {1, 2, 3, 4, 5}
# {c, c, d, d, d}

#######################################################
# Generamos nuestro espacio de estados, elegimos un espacio inicial al azar
# y también generamos nuestra matriz de transición P
espacio_estados   <- seq(from=1, to=c+d)
estado_inicial    <- sample(x = espacio_estados, size = 1)
matriz_transicion <- matrix(data = 0, nrow = c+d, ncol = c+d)
matriz_transicion[1:c,(c+1):(c+d)] <- 1/d
matriz_transicion[(c+1):(c+d),1:c]     <- 1/c

#######################################################
# Distrbución Límite // Elevar la matriz a una potencia muy grande

# La matriz toma una forma (u otra) dependiendo si la potencia es par o impar
matrixcalc::matrix.power(x = matriz_transicion, k = 1001) 
matrixcalc::matrix.power(x = matriz_transicion, k = 1002)

#######################################################
# Distrbución Estacionaria // Dejar "corriendo" la cadena
set.seed(123) # Dejamos una semilla para reproducir el experimento
num_pasos     <- 1000000
cadena_markov <- vector(mode = "numeric", length = num_pasos)
estado_actual <- estado_inicial
for( k in seq(from=1, to=num_pasos) ){
  estado_actual    <- sample(x = espacio_estados, size = 1, prob = matriz_transicion[estado_actual,] )
  cadena_markov[k] <- estado_actual
}
cadena_markov <- c(estado_inicial, cadena_markov)

# Número de veces que estuvo en cada estado
table(cadena_markov)

# Proporción del tiempo que estuvo en cada estado
prop.table(table(cadena_markov))

# Recordemos que el espacio de estados es el siguiente
# (1, 2, 3, ..., c, c+1, c+2, ..., c+d)
# donde hay "c" estados al inicio y "d" estados al final
# Si nos percatamos, la proporción del tiempo se puede obtener de la siguiente manera
# digamos que del 100% de probabilidad, 50% le corresponde a los primeros "c" estados
# y el 50% de probabilidad a los otros "d" estados
# entonces la proporción del tiempo a la que convergen debe ser la siguiente
# 1/(2c) para los primeros "c" estados y 1/(2d) para los "d" estados

# por ejemplo, si se ejecuta el código tal cual está, deberían obtener los siguientes resultados
# PROPORCION DEL TIEMPO DE CADA ESTADO ###################
# 1         2         3         4         5 
# 0.2505975 0.2493975 0.1678283 0.1654583 0.1667183 
# podemos ver que 1/(2c) = 1/4 = 0.25 mientras que 1/(2d) = 1/6 = 0.166666

# S = {1, 2, 3, ..., c, c+1, c+2, ..., c+d }
# pi = {1/(2c), 1/(2c), 1/(2c), ..., 1/(2c), 1/(2d), 1/(2d), 1/(2d), ..., 1/(2d) }

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################