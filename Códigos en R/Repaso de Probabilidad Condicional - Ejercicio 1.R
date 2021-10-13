####################################################################################################
# Ejercicio 1
####################################################################################################

n_simulaciones <- 50000  # Número de veces que se repetirá el experimento
historico      <- vector("numeric",n_simulaciones) # Vector donde guardaremos las simulaciones

n      <- 50  # Número de pisos
lambda <- 50  # Parámetro de la distribución Poisson
for( sim in 1:n_simulaciones ){
  Y   <- rpois(n = 1, lambda = lambda)                 # Número de personas que suben al elevador
  X_Y <- rbinom(n = 1, size = n, prob = 1-(1-1/n)^Y )  # Número de paradas del elevador dado X
  historico[sim] <- X_Y                                # Guardamos el resultado en nuestro vector
}
mean(historico)      # Estimación por Simulación
n*(1-exp(-lambda/n)) # Resultado Real por Álgebra

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################