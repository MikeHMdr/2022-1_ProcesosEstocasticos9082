################################################################################
# PROCESO POISSON COMPUESTO
################################################################################

NUM_SIMULACIONES <- 50000
HISTORICOS_PAGOS <- vector(mode = "numeric", length = NUM_SIMULACIONES)
for( INDICE in seq(NUM_SIMULACIONES) ){
  
  # poisson | binomial negativa (no negativa y discreta)
  NUMERO_ACCIDENTES <- rpois(n = 1, lambda = 100)
  
  # normal / gamma / etc.. ( a tu gusto)
  MONTOS_ACCIDENTES <- rgamma(n = NUMERO_ACCIDENTES, shape = 12, rate = 10)
  
  # Sumamos solamente!!
  TOTAL_A_PAGAR     <- sum(MONTOS_ACCIDENTES) # Esta es la var. aleatoria Poisson Compuesta!!!
  HISTORICOS_PAGOS[INDICE] <- TOTAL_A_PAGAR
}

# Histograma, función de densdad estimada y sobreponemos una curva normal
hist(x = HISTORICOS_PAGOS, breaks = 50, probability = TRUE)
lines(density(HISTORICOS_PAGOS), col="red", lwd=3)
curve( dnorm(x = x, mean = mean(HISTORICOS_PAGOS), sd = sd(HISTORICOS_PAGOS) ),
       from=60, to = 180, add=TRUE, col="purple", lwd=3 )
abline(v=4500, col="purple", lwd=3)
# Intervalo de confianza al 100%-alpha
ALPHA <- 0.05
quantile(x = HISTORICOS_PAGOS, probs = c(ALPHA/2, 1-ALPHA/2))



################################################################################
# FIN DEL ARCHIVO
################################################################################