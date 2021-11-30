####################################################################################################
# DISTRIBUCIÓN LIMITE - EJERCICIO 4 ( COMPROBACION DE ESTACIONALIDAD )
####################################################################################################

# SE CREA LA MATRIZ P_infinito
P_infinity <- matrix(data = c(1,0,0,1/2,0,1/2,0,0,1), nrow = 3, ncol = 3, byrow = TRUE)
distr_lim  <- c(-pi,0,exp(1))
distr_lim %*% P_infinity == distr_lim

# TAMBIEN PODRIAMOS PROBAR CON VALORES ALEATORIOS PARA COMPROBARLO
# DEFINAMOS UNA FUNCION MUY SIMPLE QUE NOS REGRESE UN 1 SI SON IGUALES (NUMERICAMENTE) Y 0 SI NO LO SON
datos_probar <- data.frame( pi_1 = runif(1E4,-1E4,1E4), pi_2 = 0, pi_3 = runif(1E4,-1E4,1E4), error_abs = 0.0 )
for( k in seq(nrow(datos_probar)) ){
  distribucion_prueba     <- as.matrix( datos_probar[k,c(1,2,3)] )
  distribucion_resultante <- distribucion_prueba %*% P_infinity
  datos_probar$error_abs[k] <- 10000 * sum( abs(distribucion_resultante - distribucion_prueba) )
}
summary(datos_probar$error_abs)

####################################################################################################
# TIEMPO PROMEDIO QUE PASA LA CADENA EN UN ESTADO
####################################################################################################

# FIJAMOS LOS VALORES DE 'a' Y 'b', Y Generamos nuestra matriz de 2 estados
a <- 1/9; b <- 3/7
P <- matrix(data = c(1-a, a, b, 1-b), nrow = 2, ncol = 2, byrow = TRUE)

# DEJEMOS CORRER LA CADENA CIERTO TIEMPO Y LUEGO CALCULEMOS LA PROPORCION DEL TIEMPO QUE PASA EN CADA ESTADO
x0            <- 1   # Estado Inicial
num_pasos     <- 500000  # Numero de pasos que se dejará correr la cadena de Markov
cadena_markov <- vector(mode = "numeric", length = num_pasos) # Vector con los estados de la cadena
x_actual <- x0
for( k in seq(num_pasos) ){
  # Generamos el siguiente estado de la cadena de acuerdo al actual
  x_siguiente      <- sample( x = c(1,2), size = 1, replace = FALSE, prob = P[x_actual,] )
  cadena_markov[k] <- x_siguiente
  x_actual         <- x_siguiente
}
# Finalmente, a la cadena le agregamos el estado inicial
cadena_markov <- c(x0, cadena_markov)

# La función 'Table' hace un conteo de cada estado
# para saber la proporcion de tiempo dividamoslo entre el número de pasos + 1
# table(cadena_markov)
proporcion_tiempo <- table(cadena_markov)/(num_pasos+1); proporcion_tiempo

# La teoría nos dice, que el porcentaje de tiempo promedio debería de converger a su distribución estacionaria
distr_estacionaria <- c( b/(a+b) , a/(a+b) ); distr_estacionaria

sum( abs(distr_estacionaria - proporcion_tiempo) ) # Error absoluto

# Pregunta :: ¿Afectará el estado inicial de la Cadena?

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################