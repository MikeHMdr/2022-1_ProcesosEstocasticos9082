####################################################################################################
# 04 - EJEMPLO DE SIMULAR EL PRECIO DE UNA ACCION
####################################################################################################

# DESCRIPCION ######################################################################################
# Funci?n que simula una trayectoria para una caminata aleatoria simple
#
# ARGUMENTOS #######################################################################################
# num_pasos    := N?mero de pasos
# probabilidad := Probabilidad de ir hacia arriba
# inicio       := Posicion inicial
####################################################################################################
simula_accion <- function(num_periodos, precio_actual, media = 0, volatilidad = 0.01){
  historico_precios    <- vector("numeric",num_periodos+1)
  historico_precios[1] <- precio_actual
  for( i in seq_len(num_periodos) ){
    rendimiento            <- exp( rnorm(1,media,volatilidad) )
    historico_precios[i+1] <- historico_precios[i] * rendimiento
  }
  plot(seq_len(num_periodos+1),historico_precios,type='n',main="Simulación del precio de una acción",
       xlab="Periodo", ylab="Precio de la acción" ); grid(lwd = 3)
  lines(seq_len(num_periodos+1),historico_precios,col="firebrick1",pch=19,lwd=3)
  # points(seq_len(num_periodos+1),historico_precios,col="royalblue",pch=19,cex=2)
}

# EJEMPLO  #########################################################################################
simula_accion(num_periodos = 100, precio_actual = 100)
####################################################################################################



####################################################################################################
# SEGUNDA PARTE
# AHORA DESEAMOS REPETIR EL MISMO EJERCICIO PERO UN NUMERO DE VECES MUY GRANDE
####################################################################################################
# SE MODIFICA UN POCO LA FUNCI?N ELIMINANDO LA GRAFICA
simula_accion <- function(num_periodos, precio_actual, media = 0, volatilidad = 0.01){
  historico_precios    <- vector("numeric",num_periodos+1)
  historico_precios[1] <- precio_actual
  for( i in seq_len(num_periodos) ){
    rendimiento            <- exp( rnorm(1,media,volatilidad) )
    historico_precios[i+1] <- historico_precios[i] * rendimiento
  }
  return( historico_precios )
}
# DEFINIMOS LA CANTIDAD DE TRAYECTORIAS QUE DESEAMOS SIMULAR Y UN OBJETO QUE LAS GUARDARA
set.seed(1)
num_simulaciones <- 2000
periodos_simular <- 100
datos_simulados  <- matrix(data = 0, nrow = num_simulaciones, ncol = periodos_simular+1)
# GENERAMOS MUCHAS TRAYECTORIAS DEL PROCESO
for( sim in seq_len(num_simulaciones) ){
  datos_simulados[sim,] <- simula_accion(num_periodos = periodos_simular, precio_actual = 100)
}
# GRAFICAMOS
plot(0,0,type='n',main="Simulaciones del precio de una acción", xlab="Periodo", ylab="Precio Acción",
     ylim=c(min(datos_simulados), max(datos_simulados)), xlim=c(1, periodos_simular+1));grid(lwd = 3)
for( sim in seq_len(num_simulaciones) ){
  lines(datos_simulados[sim,],col=sim,pch=19,lwd=1)
}
alpha<-0.05
quantile( datos_simulados[,40], probs = c(alpha/2,1-alpha/2) )
abline(h=95.94, col="red", lwd=3)
abline(h=104.44, col="red", lwd=3)

abline(h=88.73041, col="purple", lwd=3)
abline(h=112.67418, col="purple", lwd=3)

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################
