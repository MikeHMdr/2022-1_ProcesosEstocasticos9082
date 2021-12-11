####################################################################################################
# Ejercicio de simulación para la tarea 4 opcional ejercicio 6
####################################################################################################
# GENERAMOS LA MATRIZ DE TRANSICIÓN
P <- matrix(data = c(0.93, 0.01, 0.02, 0.04, 0.00, 0.00, 0.00,
                     0.00, 0.94, 0.00, 0.00, 0.02, 0.04, 0.00,
                     0.00, 0.00, 0.95, 0.00, 0.01, 0.00, 0.04,
                     0.00, 0.00, 0.00, 0.97, 0.00, 0.01, 0.02,
                     1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                     1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                     1.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00), nrow = 7, ncol = 7, byrow = TRUE )

# VALIDAMOS QUE SUME 1 POR FILA
rowSums(P)

# --------------------------------------------------------------------------------------------------
# TIP ::  Usualmente en programación este tipo de 'validaciones' se les llaman 'asserts'
# Que se puede traducir como 'afirmaciones'. Se usa usualmente para afirmar una condición
# En caso de que se cumpla la condición, el proceso continua sin hacer nada
# y en caso de que no se cumpla se marca un error. Una forma de implementarlo es la siguiente
stopifnot( all( rowSums(P) == 1 ) )
# Vease que al ejecutarse no muestra ni siquiera un mensaje porque la afirmación es correcta
# Esto sirve para codigos muy largos que a veces es dificl rastrear el error.
# Es una buena práctica hacer 'asserts' para asegurarse del correcto funcionamiento de su código
# --------------------------------------------------------------------------------------------------

# PARAMETROS GENERALES DEL EJERCICIO ###############################################################
set.seed(1)                                                 # SEMILLA PARA REPLICAR RESULTADOS
estados   <- c("0","1","2","3","12","13","23")              # VECTOR CON LOS POSIBLES ESTADOS
num_pasos <- 1800                                           # NUMERO DE PASOS DE LA CADENA
cadena    <- vector(mode = "character", length = num_pasos) # GENERAMOS UNA CADENA "VACIA"
estado_actual <- "0"                                        # ESTADO INICIAL DE LA CADENA
for( t in seq(num_pasos) ){
  # IMPLEMENTACION (PARA NADA EFICIENTE) DE COMO SELECCIONAR EL RENGLON CORRESPONDIENTE
  # DE LA MATRIZ DE TRANSICION DEPENDIENDO DEL ESTADO ACTUAL DE LA CADENA
  if( estado_actual == "0"  ) vector_probabilidad <- P[1, ]
  if( estado_actual == "1"  ) vector_probabilidad <- P[2, ]
  if( estado_actual == "2"  ) vector_probabilidad <- P[3, ]
  if( estado_actual == "3"  ) vector_probabilidad <- P[4, ]
  if( estado_actual == "12" ) vector_probabilidad <- P[5, ]
  if( estado_actual == "13" ) vector_probabilidad <- P[6, ]
  if( estado_actual == "23" ) vector_probabilidad <- P[7, ]
  # GENEAMOS UN NUEVO ESTADO A PARTIR DE UN MUESTREO DE LOS ESTADOS PONDERADO POR SU PROBABILIDAD
  estado_nuevo  <- sample(x = estados, size = 1, prob = vector_probabilidad )
  # GUARDAMOS EL NUEVO ESTADO GENERADO EN LA CADENA DE MARKOV Y ACTUALIZAMOS EL ESTADO ACTUAL
  cadena[t]     <- estado_nuevo
  estado_actual <- estado_nuevo
}

# LA FUNCION 'table' NOS MUESTRA UNA 'TABLA DE FRECUENCIAS' DE LOS DATOS
# PODEMOS USARLA PARA CUANTIFICAR LA CANTIDAD DE VECES QUE SE REPITI? CADA ESTADO DE LA CADENA
tabla_frecuencia <- table(cadena)
# print( tabla_frecuencia )
tabla_final <- prop.table(tabla_frecuencia)*1800
tabla_final

tabla_final["12"] + tabla_final["13"]
tabla_final["12"] + tabla_final["23"]
tabla_final["13"] + tabla_final["23"]
# SUPONGAMOS QUE LA CADENA TIENE EL SIGUIENTE COMPORTAMIENTO PARA 10 PASOS
# 0, 1, 1, 12, 0, 0, 3, 3, 13, 0
# VEAMOS QUE EL NUMERO DE PIEZAS QUE USO EN TODO ESTOS 10 TIEMPOS
# NO ES MUY COMPLICADO DE VER QUE USO SOLO 4 PIEZAS!!!!
# Piezas Tipo '1' = 2
# Piezas Tipo '2' = 1
# Piezas Tipo '3' = 1
# ¿POR QUÉ? PORQUE LOS ESTADOS "1","2","3" NO DEBERIAN CONTAR BAJO ESTE CONTEXTO
# DIA  1 :: SE EMPEZO CON LA MAQUINA FUNCIONANDO
# DIA  2 :: SE DESCOMPONE LA PIEZA 1
# DIA  3 :: NO SE ARREGLO NADA, SIGUE SIN FUNCIONAR LA PIEZA 1
# DIA  4 :: NO SE ARREGLO NADA, PERO AHORA TAMBI?N SE DESCOMPUSO LA PIEZA 2
# DIA  5 :: SE ARREGLARON AMBAS PIEZAS Y AL FINAL DEL DIA LA MAQUINA SIGUE FUNCIONANDO
# DIA  6 :: NO SE ESTROPEO NINGUNA PIEZA
# DIA  7 :: SE DESCOMPONE LA PIEZA 3
# DIA  8 :: NO SE ARREGLO NADA, SIGUE SIN FUNCIONAR LA PIEZA 3
# DIA  9 :: NO SE ARREGLO NADA, PERO AHORA TAMBI?N SE DESCOMPUSO LA PIEZA 1
# DIA 10 :: SE ARREGLARON AMBAS PIEZAS Y AL FINAL DEL DIA LA MAQUINA SIGUE FUNCIONANDO

# RESULTADOS QUE LES DEBERÍA DAR CON USANDO UNA SEMILLA set.seed(1) 
# ------------------------------------------
#  "0"   "1" "12"  "13"  "2" "23"  "3"
# 517    59    4    15  237   21  947
# ------------------------------------------

# PARA ESTE EJEMPLO, EL NUMERO DE PIEZAS QUE SE REEMPLAZARON FUERON 2 * ( 4 + 15 + 21) = 2 * 40 = 80
# DE LAS CUALES FUERON 19 PIEZAS "1", 25 PIEZAS "2" Y 36 PIEZAS "3"
1 / ( 4/num_pasos) # CADA 450 PASOS REGRESAMOS AL ESTADO "12"
1 / (15/num_pasos) # CADA 120 PASOS REGRESAMOS AL ESTADO "13"
1 / (21/num_pasos) # CADA ~86 PASOS REGRESAMOS AL ESTADO "23".
# DE ESTOS ESTADOS QUE NOS INTERESAN, EL ESTADO "23" ES EL QUE PASA CON MAYOR FRECUENCIA
# PARA DETERMINAR LA RESPUESTA DEL EJERCICIO, SOLO BASTA DETERMINAR N_n(y)/n PARA CADA UNO DE ESTOS
# PIENSEN ?QUE SIGNIFICA N_n(y)/n? EN LUGAR DE CALCULARLO DIRECTAMENTE, CALCULEN 1/MU(Y)

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################