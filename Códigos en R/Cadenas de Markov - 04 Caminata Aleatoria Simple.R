####################################################################################################
# 01 - EJEMPLO DE CAMINATA ALEATORIA SIMPLE
####################################################################################################

# DESCRIPCION ######################################################################################
# Función que simula una trayectoria para una caminata aleatoria simple
#
# ARGUMENTOS #######################################################################################
# num_pasos    := Número de pasos
# probabilidad := Probabilidad de ir hacia arriba
# inicio       := Posicion inicial
####################################################################################################
caminata_simple <- function(num_pasos, probabilidad = 1/2, inicio = 0){
  saltos     <- sample(x = c(1,-1), size = num_pasos, replace = TRUE, prob = c(probabilidad, 1-probabilidad) )
  posiciones <- c(inicio, inicio + cumsum(saltos) )
  plot(seq(from=0, to=num_pasos),posiciones,type='n',main="Caminata Aleatoria Simple",
       xlab="Espacio Parametral [n]", ylab="Espacio de Estados [X_n]" ); grid(lwd = 3)
  lines(seq(from=0, to=num_pasos),posiciones,col="firebrick1",pch=19,lwd=3)
  points(seq(from=0, to=num_pasos),posiciones,col="royalblue",pch=19,cex=2)
}

# EJEMPLO  #########################################################################################
set.seed(1)
caminata_simple(num_pasos = 500, probabilidad = 1/2, inicio = 0)
####################################################################################################

