####################################################################################################
# Ejercicio 2
####################################################################################################

n_simulaciones <- 50000 # Número de veces que se repetirá el experimento
historico_X    <- vector("numeric",n_simulaciones) # Vector donde guardaremos las simulaciones de X
historico_Y    <- vector("numeric",n_simulaciones) # Vector donde guardaremos las simulaciones de Y

lambda <- 100 # Parámetro de Poisson. (Valor desconocido pero fijo, no aleatorio)
p      <- 0.5 # Probabilidad de éxito para que el huevo sea fecundado
for( sim in 1:n_simulaciones){
  X   <- rpois(n = 1, lambda = lambda)     # Cantidad de huevos
  Y_X <- rbinom(n = 1, size = X, prob = p) # Cantidad de pollitos (huevos fecundados)
  historico_X[sim] <- X                    # Guardamos los resultados de X
  historico_Y[sim] <- Y_X                  # Guardamos los resultados de Y|X
}
cor(x = historico_X, y = historico_Y)      # Estimación por Simulación
sqrt(p)                                    # Resultado Real por Álgebra

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################