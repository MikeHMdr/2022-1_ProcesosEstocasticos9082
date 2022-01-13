####################################################################################################
# EJEMPLO DE PROCESO POISSON NO HOMOGENEO
####################################################################################################

# Parámetros Globales
set.seed(1)
lambda  <- function(t) 1 * ( sin( t*2*pi/48 - pi/2 ) + 1 ) / 2
tiempos <- seq(from=0, to=24*3, by = 1)
plot(tiempos, lambda(tiempos), type="l", col="red", main="lambda(x)", xlab="x", ylab="Lambda(x)" )
plot(tiempos, cumsum(lambda(tiempos)), type="l", col="red", main="Lambda(t)", xlab="t", ylab="Tasa" )

# Se simulan los eventos en cada sub intervalo de tiempo
num_periodos <- length(tiempos)-1
num_simulaciones <- vector(mode = "numeric",length = num_periodos)
simulaciones_exp <- NULL
for( t in seq( num_periodos ) ){
  tasa_t <- integrate( f = lambda, lower = tiempos[t], upper = tiempos[t+1] )[["value"]]
  # Se simulan la cantidad de "eventos" en el total de tiempo
  num_simulaciones[t] <- rpois(n = 1, lambda = tasa_t )
  # Se simulan los tiempos "inter-arrivo" de cada evento
  simulaciones_exp <- c(simulaciones_exp, rexp(n = num_simulaciones[t], rate = tasa_t ) + (t-1) )
}
simulaciones_exp <- sort( simulaciones_exp )

# Generamos los vectores que representen las coordenadas de los puntos
valores_x <- simulaciones_exp # Tiempos en los que se presentan los eventos
valores_y <- c( 0 , seq( sum(num_simulaciones) ) ) # Altura de la función
function_a_trozos <- stepfun(x = valores_x, y = valores_y, f = 0 )

# Gráfica del proceso
plot(function_a_trozos, main="Proceso Poisson No Homogéneo", verticals = FALSE,
     pch=19, col.points ="red", xlim=c(0, max(tiempos)), xlab = "Tiempo", ylab = "Eventos")
lines(tiempos, cumsum(lambda(tiempos)), type="l", col="purple", lwd=2)

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################