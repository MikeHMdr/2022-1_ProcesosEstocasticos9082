####################################################################################################
# Ejercicio de simulación para la tarea 4 opcional ejercicio 5
####################################################################################################
# GENERAMOS LA MATRIZ DE TRANSICIÓN
P <- matrix(data = c(0.8, 0.2, 0.0, 0.0,
                     0.4, 0.6, 0.0, 0.0,
                     0.0, 0.0, 0.1, 0.9,
                     0.0, 0.0, 0.6, 0.4), nrow = 4, ncol = 4, byrow = TRUE )

# VALIDAMOS QUE SUME 1 POR FILA
rowSums(P)

# PARAMETROS GENERALES DEL EJERCICIO ###############################################################
set.seed(1)                                                       # SEMILLA PARA REPLICAR RESULTADOS
num_pasos       <- 100000                                         # NUMERO DE PASOS DE LA CADENA
espacio_estados <- c(1,2,3,4)                                     # VECTOR CON LOS POSIBLES ESTADOS
cadena_markov   <- vector(mode = "numeric", length = num_pasos)   # GENERAMOS UNA CADENA "VACIA"
estado_actual   <- sample(x = espacio_estados, size = 1)          # ESTADO INICIAL DE LA CADENA
for( k in seq(num_pasos) ){
  # GENEAMOS UN NUEVO ESTADO A PARTIR DE UN MUESTREO DE LOS ESTADOS PONDERADO POR SU PROBABILIDAD
  estado_actual  <- sample(x = espacio_estados, size = 1, prob = P[estado_actual,] )
  # GUARDAMOS EL NUEVO ESTADO GENERADO EN LA CADENA DE MARKOV Y ACTUALIZAMOS EL ESTADO ACTUAL
  cadena_markov[k] <- estado_actual
}

# LA FUNCION 'table' NOS MUESTRA UNA 'TABLA DE FRECUENCIAS' DE LOS DATOS
# PODEMOS USARLA PARA CUANTIFICAR LA CANTIDAD DE VECES QUE SE REPITI? CADA ESTADO DE LA CADENA
tabla_frecuencia <- table(cadena)
# print( tabla_frecuencia )
tabla_final <- prop.table(tabla_frecuencia)
tabla_final

# ¿PUEDEN ENCONTRAR UNA FORMA ALGEBRAICA PARA ENCONTRAR EL VALOR EXACTO DONDE CONVERGEN?
# INTENTEN USAR LA DEFINICION DE DISTRIBUCION ESTACIONARIA!!

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################