####################################################################################################
# EJERCICIO 2 DE LAS NOTAS PDF
####################################################################################################
# Parámetros Globales
set.seed(1)        # Semilla de números pseudo-aleatorios
lambda   <- 10     # Num. Promedio de Eventos por Unidad de Tiempo (Días en este caso)
tiempo_t <- 100000 # Tiempo que "observaremos" el Proceso Poisson en Días (14 días = 2 Semanas)
tiempo_a <- 0.1    # 1/10 minutos = 60/10 segundos = 6 segundos

# Se simulan la cantidad de "autos" en el total de tiempo
num_simulaciones <- rpois(n = 1, lambda = lambda*tiempo_t)

# Se simulan los tiempos "inter-arrivo" de cada evento
# Los cuales se suman para obtener los tiempos en los que se presentan los eventos
simulaciones_exp <- rexp(n = num_simulaciones, rate = lambda)
# simulaciones_exp <- round(simulaciones_exp,3)


# Se analiza el tiempo que tuvo que estar esperando
indices     <- which(simulaciones_exp > tiempo_a)
indices_inf <- c(1, indices+1)
indices_sup <- indices-1
vector_tiempos <- vector("numeric",length(indices))
for( k in seq(from=2, to=length(indices)) ){
  indices_tmp         <- seq(indices_inf[k], indices_sup[k])
  vector_tiempos[ k ] <- sum( simulaciones_exp[ indices_tmp ] ) #+ tiempo_a
}

( 1-(tiempo_a*lambda+1)*exp(-tiempo_a*lambda) )/lambda

hist(vector_tiempos, col="#1CACDB", main="Tiempos de Espera", breaks = 100, probability = TRUE )
hist(simulaciones_exp, col="#1CACDB", main="Tiempos Inter-arribo", breaks = 100, probability = TRUE )
lines(curve(dexp(x,lambda),add=TRUE, col="red", lwd=3))
abline(v=tiempo_a, col="orange", lwd=3)


####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################