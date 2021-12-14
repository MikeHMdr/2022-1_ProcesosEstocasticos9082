#############################################################
# CADENA DE EHRENFEST
#############################################################

### PARAMETROS INICIALES
N       <- 300
PELOTAS <- seq(from=1, to=N, by=1)
x0      <- 20
CAJA_A  <- sample(x = PELOTAS, size = x0)
CAJA_B  <- PELOTAS[-CAJA_A]

### SIMULACIONES
CADENA_XN   <- c(x0)
ITERACIONES <- 5000
for( k in seq(from=1, to=ITERACIONES)){
  PELOTA_ELEGIDA <- sample(x = PELOTAS, size = 1)
  if( PELOTA_ELEGIDA %in% CAJA_A ){
    CAJA_A <- setdiff(CAJA_A, PELOTA_ELEGIDA)
    CAJA_B <- c( CAJA_B, PELOTA_ELEGIDA )
  }else{
    CAJA_A <- c( CAJA_A, PELOTA_ELEGIDA )
    CAJA_B <- setdiff(CAJA_B, PELOTA_ELEGIDA)
  }
  CADENA_XN <- c( CADENA_XN, length(CAJA_A) )
}

plot(CADENA_XN, type="l"); grid()
abline(h=N/2,col="red",lwd=3)
abline(h=mean(CADENA_XN),col="purple",lwd=3)
# Xn = "Numero de pelotas en la Caja A"

#############################################################
# FIN DEL ARCHIVO
#############################################################