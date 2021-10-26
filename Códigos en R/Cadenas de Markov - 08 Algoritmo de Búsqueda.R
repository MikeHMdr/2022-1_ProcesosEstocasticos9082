################################################################################
# Código para resolver el ejercicio 3 de la tarea 2
################################################################################

algoritmo_busqueda_cero <- function(cantidad_de_estados = 10){
  conjunto_de_estados <- seq(from=0, to=cantidad_de_estados)
  estado_actual       <- cantidad_de_estados
  cadena_markov       <- c()
  while(TRUE){
    probabilidad_transicion <- vector(mode = "numeric", length = cantidad_de_estados+1)
    probabilidad_transicion[1:estado_actual] <- 1/estado_actual
    estado_actual <- sample(x = conjunto_de_estados, size = 1, prob = probabilidad_transicion)
    cadena_markov <- c( cadena_markov, estado_actual )
    if( estado_actual == 0 ) break()  
  }
  cadena_markov <- c(cantidad_de_estados, cadena_markov)
  tiempo        <- length(cadena_markov)-1
  return(list("Cadena de Markov"=cadena_markov, "Tiempo de Búsqueda"=tiempo))
}

num_simulaciones <- 30000
resultados       <- vector(mode = "numeric", length = num_simulaciones)
for( sim in seq(num_simulaciones) ){
  resultados[sim] <- algoritmo_busqueda_cero(cantidad_de_estados = 150)$`Tiempo de Búsqueda`
}
mean(resultados)
1   -> 1
2   -> 1.5052
3   -> 1.8445
4   -> 2.08238
5   -> 2.287533
8   -> 2.7042
10  -> 2.929633
15  -> 3.319467
30  -> 3.993433
80  -> 4.976033
150 -> 5.580567

algoritmo_busqueda_cero(cantidad_de_estados = 30)

################################################################################
# fin del archivo
################################################################################