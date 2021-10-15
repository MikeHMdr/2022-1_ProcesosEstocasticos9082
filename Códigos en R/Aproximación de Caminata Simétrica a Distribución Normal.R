####################################################################################################
# APROXIMACIÓN DEL MOVIMIENTO BROWNIANO
# EL PROPÓSITO DE ESTE CODIGO ES VISUALIZAR EL COMPORTAMIENTO DE LA CAMINATA ALEATORIA
# SIMÉTRICA CUANDO DELTA TIENDE A CERO, Y VER QUE ESTO SE DISTRIBUYE NORMAL
####################################################################################################
set.seed(1)

# Función para simular una posible trayectoria de un browniano estándar por medio de uniformes
simula_browniano <- function(tiempo, delta){
  particion_t <- seq( from = 0 , to = tiempo, by = delta )
  num_pasos   <- length(particion_t)
  vars_auxs   <- sqrt(delta_t) * sample(x = c(-1,1), size = num_pasos-1, replace = TRUE)
  mov_brown   <- c(0, cumsum( vars_auxs ) )
  return( mov_brown )
}

# Parámetros generales
tiempo_t    <- 3
delta_t     <- 0.001
num_sims    <- 50000
confianza   <- 0.05

# Simulaciones y cálculo de los intervalos de confianza
matriz_sims <- replicate(n = num_sims, expr = simula_browniano(tiempo = tiempo_t, delta = delta_t) )
seq_ejex    <- seq( from = 0 , to = tiempo_t, by = delta_t )
lim_sup     <- apply(X = matriz_sims, MARGIN = 1, FUN = quantile, probs=1-confianza/2 )
lim_inf     <- apply(X = matriz_sims, MARGIN = 1, FUN = quantile, probs=  confianza/2 )
lim_sup_t   <- sqrt( qnorm(p = 1-confianza/2, mean = 0, sd = 2*seq_ejex ) )
lim_inf_t   <- -lim_sup_t # Debido a la simetría de la normal
limite_abs  <- max(abs(  c(min(lim_inf),max(lim_sup)) ))
# Grafica del proceso
plot(x = 0, y = 0, type = "n", ylim = c(-limite_abs,limite_abs), xlim=c(0,tiempo_t),
     main = "Aproximación del movimiento browniano", xlab="t", ylab="W(t)"); grid()
for( k in seq(ncol(matriz_sims))){
  lines(x = seq_ejex, y = matriz_sims[,k], col=ggplot2::alpha(colors()[k],0.5), lwd=1 )
}
lines(x = seq_ejex, y = lim_sup_t, lwd = 2, col="red")
lines(x = seq_ejex, y = lim_inf_t, lwd = 2, col="red")

# ¿Cuál es la probabilidad de que 0<=W(2)<=1?
segments(x0 = 2, y0 = 0, x1 = 2, y1 = 1, col = "blue", lwd=3)
vector_sims <- matriz_sims[ which(seq_ejex==2), ]
cat(paste0("La probabilidad es ", mean( 0 <= vector_sims & vector_sims <= 1 ) ))
# pnorm(q = 1, mean = 0, sd = sqrt(2) )-pnorm(q = 0, mean = 0, sd = sqrt(2) )
# hist(vector_sims, breaks = 50, probability = TRUE, main="Simulaciones de Browniano Estándar W(5)",
#      xlab="Valor del W(t)", ylab="Probabilidad", col="#1CACDB")
# curve( dnorm(x, 0, sqrt(2) ),add=TRUE, col="red", lwd=3)


# ¿Cuál es la probabilidad de que 0<=W(2)<=1 | W(1)=-1?
points(x = 1, y = -1, col="blue", pch=19, cex=2)
vector_sims1 <- matriz_sims[ which(seq_ejex==1), ]
vector_sims2 <- matriz_sims[ which(seq_ejex==2), ]
vector_sims  <- vector_sims2[ abs( vector_sims1 - (-1) ) <= 0.02 ]
# vector_sims <- matriz_sims[ which(seq_ejex==2),  ]
cat(paste0("La probabilidad es ", mean(0 <= vector_sims & vector_sims <= 1) ))
# hist(vector_sims, breaks = 50, probability = TRUE, main="Simulaciones de Browniano Estándar W(5)",
#      xlab="Valor del W(t)", ylab="Probabilidad", col="#1CACDB")
# curve( dnorm(x, 0, sqrt(2) ),add=TRUE, col="red", lwd=3)


####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################