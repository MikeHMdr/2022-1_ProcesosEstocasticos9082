####################################################################################################
# LA SEÑAL DEL TELEGRAFO
####################################################################################################

# set.seed(1)        # Semilla de números pseudo-aleatorios
num_sims <- 1E5    # Número de Simulaciones
# lambda   <- 0.2    # Parámetro Lambda !! (Jugar con Lambda < 1 y Lambda >= 1)
# lambda   <- runif(n = 1, min = 1, max = 5)
lambda <- 2
# exp(-lambda)*cosh(x = lambda)

# Generación de las variables aleatorias
N <- rpois(n = num_sims, lambda = lambda)                 # Distribución Poisson
X <- (-1)**N                                              # Señal Semi-Aleatoria
Z <- sample(x = c(-1,1), size = num_sims, replace = TRUE) # Uniforme Discreta {-1, 1}
Y <- Z * X                                                # Señal Aleatoria

# mean( N %% 2 == 0 )

# Medias de las distribuciones
print(paste0( "Media de X = ", mean(X) ))
print(paste0( "Media de Z = ", mean(Z) ))
print(paste0( "Media de Y = ", mean(Y) ))

# Funciones de densidad
print(lambda)
# barplot(prop.table(table(N)), main="Densidad de N", col="brown1")
barplot(prop.table(table(X)), main="Densidad de X", col=c("brown1","deepskyblue4") )
# barplot(prop.table(table(Z)), main="Densidad de Z", col="deepskyblue4")
barplot(prop.table(table(Y)), main="Densidad de Y", col=c("brown1","deepskyblue4") )

# auto-correlation function
acf(X)
acf(Y)
####################################################################################################
# FIN DEL ARCHIVO
####################################################################################################