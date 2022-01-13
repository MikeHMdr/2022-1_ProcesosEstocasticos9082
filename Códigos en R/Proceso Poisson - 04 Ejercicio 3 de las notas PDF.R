####################################################################################################
# EJERCICIO 3 DE LAS NOTAS PDF
####################################################################################################
# Parámetros Globales
set.seed(1)        # Semilla de números pseudo-aleatorios
lambda   <- 2      # Num. Promedio de Eventos por Unidad de Tiempo (Días en este caso)
tiempo_t <- 7      # Tiempo que "observaremos" el Proceso Poisson en Días (7 días = 1 Semana)
num_sims <- 50000  # Número de simulaciones para estimar la media y varianza

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Primera Parte :: Estimación de la Esperanza y Varianza
# Se simulan la cantidad de "eventos" en el total de tiempo y se estima la media y varianza
vector_simulaciones <- vector(mode = "numeric", length = num_sims)
for( sim in seq(num_sims) ){
  vector_simulaciones[ sim ] <- rpois(n = 1, lambda = lambda*tiempo_t)
}
media_estimada    <- mean(vector_simulaciones); media_estimada    # 14.02228
varianza_estimada <- var(vector_simulaciones); varianza_estimada  # 14.04742

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Segunda Parte :: Modelación la cantidad de individuos en EEUU en 2 semanas
set.seed(1)        # Semilla de n?meros pseudo-aleatorios
lambda   <- 2      # Num. Promedio de Eventos por Unidad de Tiempo (D?as en este caso)
tiempo_t <- 7*2    # Tiempo que "observaremos" el Proceso Poisson en D?as (14 días = 2 Semanas)

# Se simulan la cantidad de "eventos" en el total de tiempo
num_simulaciones <- rpois(n = 1, lambda = lambda*tiempo_t)

# Se simulan los tiempos "inter-arrivo" de cada evento
# Los cuales se suman para obtener los tiempos en los que se presentan los eventos
simulaciones_exp <- rexp(n = num_simulaciones, rate = lambda)
tiempos_llegada  <- cumsum(simulaciones_exp)

# Se simulan los tama?os de las familias de cada evento
# Los cuales se suman para obtener la altura de la función
simulaciones_fam    <- sample(x = c(1,2,3,4), size = num_simulaciones, replace = TRUE,
                              prob = c(1/6,1/3,1/3,1/6) )
cantidad_individuos <- cumsum(simulaciones_fam)

# Generamos los vectores que representen las coordenadas de los puntos y graficamos
valores_x <- tiempos_llegada
valores_y <- c( 0 , cantidad_individuos )  # (se a?ade la altura 'inicial')
function_a_trozos <- stepfun(x = valores_x, y = valores_y, f = 0)
plot(function_a_trozos, main="Inmigrantes a EU", verticals = FALSE, pch=19, col.points ="red",
     lwd=1.5, cex.points=1.1, xlim=c(0, tiempo_t), xlab = "Días", ylab = "Individuos")
print(cantidad_individuos[length(cantidad_individuos)])

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################