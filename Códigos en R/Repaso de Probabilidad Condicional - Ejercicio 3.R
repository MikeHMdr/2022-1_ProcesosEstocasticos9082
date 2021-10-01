####################################################################################################
# Ejercicio 3
####################################################################################################

n_simulaciones <- 50000 # Número de veces que se repetirá el experimento
historico_P    <- vector("numeric",n_simulaciones) # Vector donde guardaremos las simulaciones de P
historico_X    <- vector("numeric",n_simulaciones) # Vector donde guardaremos las simulaciones de X

n     <- 100 # Número de partidas                -- sample(x = 1:100, size = 1)
alpha <-   1 # Parámetro de la distribución Beta -- sample(x = 1:10, size = 1)
beta  <-   1 # Parámetro de la distribución Beta -- sample(x = 1:10, size = 1)
for( sim in 1:n_simulaciones){
  P   <- rbeta(n = 1, shape1 = alpha, shape2 = beta) # Simulamos la probabilidad P de ganar
  X_P <- rbinom(n = 1, size = n, prob = P)           # Contamos el número de partidas ganadas
  historico_P[sim] <- P                              # Guardamos el valor de P
  historico_X[sim] <- X_P                            # Guardamos el valor de X|P
}
cor(x = historico_X, y = historico_P)                # Estimación por Simulación
sqrt(n)/sqrt(n+alpha+beta)                           # Resultado Real por Álgebra


# Crearemos una función de la correlación que dependa de n
# para demostrar que la correlación aumenta conforme n aumenta
rho   <- function(n, alpha, beta) sqrt(n/(n+alpha+beta))
n_seq <- seq(from=0.05,to = 10, by = 0.05)
plot( x = n_seq, y = rho(n = n_seq, alpha = alpha, beta = beta), type = "l",
      main = "Correlación entre X y P", xlab = "n", ylab = "rho(n)", lwd=2, col="red"); grid()

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################