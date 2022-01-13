####################################################################################################
# EJEMPLO DE SIMULACIÓN DE PROCESO POISSON
# METODO 1 - TIEMPOS INTER-ARRIBO
####################################################################################################

# Parámetros Globales
lambda   <- 1           # Num. Promedio de Eventos por Unidad de Tiempo
tiempo_t <- 20          # Tiempo que "observaremos" el Proceso Poisson

# Se simulan la cantidad de "eventos" en el total de tiempo
num_simulaciones <- rpois(n = 1, lambda = lambda*tiempo_t)

# Se simulan los tiempos "inter-arrivo" de cada evento
simulaciones_exp <- rexp(n = num_simulaciones, rate = lambda)

# Generamos los vectores que representen las coordenadas de los puntos
valores_x <- cumsum(simulaciones_exp)        # Tiempos en los que se presentan los eventos
valores_y <- c( 0 , seq(num_simulaciones) )  # Altura de la función (se añade la altura 'inicial')
function_a_trozos <- stepfun(x = valores_x, y = valores_y, f = 0)

# Gráfica de la Cadena de Markov
plot(function_a_trozos, main="Proceso Poisson - Método 1", verticals = FALSE, pch=19, col.points ="red",
     xlim=c(0, tiempo_t), xlab = "Tiempo", ylab = "Eventos")

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################