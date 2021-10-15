####################################################################################################
# CAMINATA ALEATORIA SIMPLE (1 SOLA TRAYECTORIA)
####################################################################################################
rm(list=rm())

# Funci?n que nos ayudar? a simular 1 sola trayectoria de una caminata aleatoria
# iniciando en 'x0' y con probabilidad 'p' de ir hacia arriba
caminata_aleatoria <- function(num_pasos, x0 = 0, p = 1/2){
  y <- sample(x = c(1, -1), size = num_pasos, replace = TRUE, prob = c(p, 1-p) )
  x <- c( x0, x0 + cumsum(y) )
  return( x )
}

# Ejemplo
caminata <- caminata_aleatoria(num_pasos = 10)
plot(caminata, type="b", pch=19, col="blue", main="Caminata Aleatoria",
     xlab="Espacio Parametral (tiempo)", ylab="Espacio de Estados" ); grid()
points(caminata, pch=19, col="blue", cex=1.5)

####################################################################################################
# CAMINATA ALEATORIA SIMPLE (N TRAYECTORIAS)
####################################################################################################
# Funci?n que nos ayudar? a simular N sola trayectoria de una caminata aleatoria
# iniciando en 'x0' y con probabilidad 'p' de ir hacia arriba.
# Utiliza la funci?n definida para el caso de 1 sola trayectoria
proceso_caminata_aleatoria <- function(num_pasos, x0 = 0, p = 1/2, num_simulaciones = 1000, graficar = TRUE){
  # GENERAMOS MUCHAS TRAYECTORIAS DEL PROCESO
  trayectorias_simuladas <- matrix(data = 0, nrow = num_simulaciones, ncol = num_pasos+1)
  for( sim in seq_len(num_simulaciones) ){
    trayectorias_simuladas[sim,] <- caminata_aleatoria(num_pasos = num_pasos, x0 = x0, p = p)
  }
  # GRAFICAMOS
  if( graficar ){
    plot(0,0,type='n',main="Simulación de Caminatas Aleatorias Simples", xlab="Espacio Parametral (tiempo)", ylab="Espacio de Estados",
         ylim=c(min(trayectorias_simuladas), max(trayectorias_simuladas)), xlim=c(0, num_pasos));grid(lwd = 2)
    for( sim in seq_len(num_simulaciones) ){
      lines(x = seq(0, num_pasos), trayectorias_simuladas[sim,],col=sim,pch=19,lwd=2)
    }
  }
  return( invisible(trayectorias_simuladas) )
}

historia <- proceso_caminata_aleatoria(num_pasos = 500, num_simulaciones = 5000, p = 0.5, graficar=FALSE)

lim_inf <- apply(X = historia, MARGIN=2, FUN = quantile, probs = 0.05/2 )
lim_sup <- apply(X = historia, MARGIN=2, FUN = quantile, probs = 1-0.05/2 )
lines(x = seq(0, 500), y = lim_inf, col="red", lwd=5)
lines(x = seq(0, 500), y = lim_sup, col="red", lwd=5)

####################################################################################################
# INTENTEMOS RESOLVER EL EJERCICIO DE LA TAREA DE FORMA VISUAL
####################################################################################################

caminata <- proceso_caminata_aleatoria(num_pasos = 10, num_simulaciones = 3000000, graficar = FALSE)
prop.table(table(caminata[,1+6]))

choose(4,1)/(2**4)
choose(9,0:9) / sum(choose(9,0:9))

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################