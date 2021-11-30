################################################################################
# Código para resolver el ejercicio 2 de la tarea 2
################################################################################

num_simulaciones  <- 1000000
historico         <- vector(mode = "numeric", length = num_simulaciones)
for( sim in seq(num_simulaciones) ){
  historico[sim] <- sum( sample(x = c(1,-1), size = 6, replace = TRUE, prob = c(1/2, 1/2) ) )
}
table(historico)/num_simulaciones





################################################################################
# fin del archivo
################################################################################