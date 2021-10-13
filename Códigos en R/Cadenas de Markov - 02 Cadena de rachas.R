####################################################################################################
# 03 - EJEMPLO DE RACHAS
####################################################################################################

# DESCRIPCION ######################################################################################
# Función que simula una trayectoria para cadena de rachas
#
# ARGUMENTOS #######################################################################################
# num_pasos    := Número de pasos
# probabilidad := probabilidad de incrementar en uno en cada paso.
####################################################################################################

# EJEMPLO    ::::::::::::::::::::::::::::::::::::::::::::::::
# RESULTADOS :: 0 1 0 1 0 0 0 1 1 0 1 1 1 1 1 1 1 1 1 0
# RACHAS     :: 0 1 0 1 0 0 0 1 2 0 1 2 3 4 5 6 7 8 9 0

cadena_rachas <- function(num_pasos, probabilidad = 1/2){
  resultados   <- sample(x = c(1,0), size = num_pasos, replace = TRUE, prob = c(probabilidad, 1-probabilidad) )
  racha_actual <- 0
  historico_rachas <- racha_actual
  for( i in seq_len(num_pasos) ){
    racha_actual <- ifelse( resultados[i] == 1, racha_actual + 1, 0 )
    historico_rachas <- c(historico_rachas, racha_actual )
  }
  racha_max <- max(historico_rachas)
  # plot(historico_rachas,type='n',main="Cadena de Rachas",
  #      xlab="Espacio Parametral [n]", ylab="Espacio de Estados [X_n]" ); grid(lwd = 3)
  # abline(h=racha_max, col="forestgreen", lwd=3)
  # lines(historico_rachas,col="firebrick1",pch=19,lwd=3)
  # points(historico_rachas,col="royalblue",pch=19,cex=2)
  return( racha_max )
}

# EJEMPLO  #########################################################################################
cadena_rachas(num_pasos = 10, probabilidad = 1/2)

rachas <- c()
for( k in seq(100000) ){
  rachas <- c(rachas, cadena_rachas(num_pasos = 10, probabilidad = 1/2) )
}

barplot(table(rachas))
round(prop.table(table(rachas))*100)
# DE 0 A 2 RACHAS = 49%
# DE 3 A 10 RACHAS = 51%

# YO GANO SI HAY 2 O 3 RACHAS = 61%
# TU GANAS SI HAY 0 O 1 RACHAS, 4 RACHAS O MÁS = 39%

####################################################################################################

