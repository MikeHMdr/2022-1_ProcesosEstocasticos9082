####################################################################################################
# DISTRIBUCI?N ESTACIONARIA - EJERCICIO 4
####################################################################################################

# GENERAMOS NUESTRA MATRIZ DE TRANSICI?N
P <- matrix(data = c(  1,  0,  0,
                     1/3,1/3,1/3,
                       0,  0,  1),
            nrow = 3,ncol = 3, byrow = TRUE)

# FIJAMOS UN ALPHA Y CREAMOS EL VECTOR CON LA DISTRIBUCI?N ESTACIAR
alpha <- runif(1)  #  pi/4
distr_estacionaria <- c(alpha, 0, 1-alpha)

# COMPROBAMOS QUE CUMPLA LA PROPIEDAD DE ESTACIONALIDAD
distr_estacionaria == distr_estacionaria %*% P


####################################################################################################
# CUALQUIER COMBINACION LINEAL COMVEXA DE DISTRIBUCIONES ESTACIONARIAS
# ES UNA DISTRIBUCI?N ESTACIONARIA

# Coeficientes de la combinación
weights <- runif(3); weights <- weights/sum(weights); weights
a1 <- 0.45348949; a2 <- 0.462198465213; a3 <- 1-a1-a2

# Par?metro Alpha de cada distribuci?n estacionaria
alpha_1 <- runif(1); alpha_2 <- runif(1); alpha_3 <- runif(1)

# Distribuciones estacionarias
distr_estacionaria_1 <- c(alpha_1, 0, 1-alpha_1)
distr_estacionaria_2 <- c(alpha_2, 0, 1-alpha_2)
distr_estacionaria_3 <- c(alpha_3, 0, 1-alpha_3)

# Generamos la nueva distribuci?n estacionaria creada como combinaci?n lineal convexa de las anteriores

# distr_estacionaria_new <- weights[1]*distr_estacionaria_1 + weights[2]*distr_estacionaria_2 + weights[3]*distr_estacionaria_3
distr_estacionaria_new <- weights[1]*distr_estacionaria_1
distr_estacionaria_new <- distr_estacionaria_new + weights[2]*distr_estacionaria_2
distr_estacionaria_new <- distr_estacionaria_new + weights[3]*distr_estacionaria_3
distr_estacionaria_new

# Demostraci?n que es una distribuci?n de probabilidad ( Entradas entre [0,1] y deben sumar 1 )
0 <= distr_estacionaria_new & distr_estacionaria_new <= 1
sum(distr_estacionaria_new)

# Demostraci?n que es una distribuci?n estacionaria ( pi = pi * P )
distr_estacionaria_new == distr_estacionaria_new %*% P

####################################################################################################
# DISTRIBUCI?N LIMITE - EJERCICIO 1 ( CASO GENERAL PARA CADENA DE 2 ESTADOS )
####################################################################################################

# FIJAMOS LOS VALORES DE 'a' Y 'b'
a <- runif(1) # 1/9
b <- runif(1) # 3/7

# Generamos nuestra matriz de 2 estados
P <- matrix(data = c(1-a, a, b, 1-b), nrow = 2, ncol = 2, byrow = TRUE)

# Como num?ricamente no podemos elevar una matriz al infinito, basicamente
# la idea es elevarla a un n?mero suficientemente grande y que cumpla condiciones de convergencia
# Para este caso simple, basta con ver que los n?meros de la matriz no cambian de una potencia a otra

# Podemos calcular la potencia de una matriz en una linea de c?digo usando al funci?n 'matrix.power'
# que se encuentra dentro de la biblioteca 'matrixcalc'
# Tambi?n dentro de 'expm' est? la funci?n '%^%' que tambi?n puede ser usara

matrixcalc::matrix.power(x = P, k = 100)
expm::`%^%`(x = P, k = 100)

# O podrian crear su propia funci?n que calcule las potencias de una matriz
elevar_matriz <- function(x, k){
  y <- x
  for( i in seq(k) ) y <- y %*% x
  return( y )
}

# Calculamos la matriz l?mite y la distribuci?n limite por m?todos num?ricos
P_lim <- elevar_matriz( x = P, k = 1000); P_lim
distr_lim_numerico <- diag(P_lim); distr_lim_numerico

# Resultado Exacto
distr_lim_exacta <- c(b/(a+b),a/(a+b))

# Comprobaci?n
distr_lim_exacta == distr_lim_numerico

# Notemos que indica que NO son iguales, pero esto es por temas de punto flotante
# Podemos aumentar los decimales que se visualizan
options(digits = 22)
distr_lim_numerico
distr_lim_exacta

# Practicamente son iguales en los primeros 14 decimales
abs( distr_lim_exacta - distr_lim_numerico ) < 1e-14 # Valor cercanos a cero

####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################