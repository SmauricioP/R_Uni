####################################
# Modelo de ecuaciones simultáneas #
####################################

# Autor: Sandro Mauricio Peirano Gozalvez

# Estimación de ecuaciones simultáneas utilizando los métodos de estimación
# visto en clase.

## Working directory ##

setwd("Econ. interm/")

#############################################################################
# Ofdem - Descripción de la data                                            #
#                                                                           #
# Se utilizará data sobre precios y cantidades de un producto agrícola para #
# estimar las ecuaciones del modelo de la telaraña.                         #
#############################################################################

library(haven)      # Abrir archivos con extensión de Stata
library(systemfit)  # Estimación de ecuaciones simultáneas

## Lectura de los datos ##

data <- read_stata("data/ofdem.dta")

############################################################################

## Especificación ##

# Se estimará el modelo telaraña,que utiliza ecuaciones de demanda y de oferta,
# donde la oferta se rezaga un periodo.

##############
# Ecuaciones #
##############

# Oferta:  q_t ~ p_{t-1}
# Demanda: p_t ~ q_t + i_t

# Al tratar de estimar un modelo de ecuaciones recursivas, tenemos la opción de
# estimar por MCO o no. Esto dependerá de la forma de la matriz de varianza de
# los errores contemporáneos.

##########################################################################3

######################
# Estimación por MCO #
######################

of <- lm(q ~ pchlag, data) # Estimación de la oferta
dm <- lm(p ~ q + i, data)  # Estimación de la demanda

# Se guardan los residuales para realizar hipótesis sobre la matriz var-cov.

of_res <- of$residuals
dm_res <- dm$residuals 

cov_sys1 <- cov(cbind(Oferta = of_res, Demanda = dm_res)) # Matriz var-cov
cor_sys1 <- cor(cbind(Oferta = of_res, Demanda = dm_res)) # Matriz corr.

###################################
# Test - independencia de errores #
###################################

# Se utiliza un test ML donde se utiliza la suma de las correlaciones que no 
# están en la diagonal. El siguiente bucle calcula el estadístico de prueba.

for (i in 1:2) {
  for (j in 1:2) {
    if (i == 1 & j == 1) {
      ml_stat <- 0
    }
    if (i < j) {
      ml_stat <- ml_stat + cor_sys1[i,j]^2
    }
    if (i == 2 & j == 2) {
      ml_stat <- nrow(data)*ml_stat
    }
  }
}

# En este caso, esta variable se distribuye asintóticamente como una chi 
# cuadrado con 1 grado de libertad. De esta manera, calculando el p-value:

p_val <- pchisq(ml_stat,1, lower.tail = FALSE); p_val

# Se observa que la hipótesis nula no se puede rechazar, por lo que se concluye
# que es probable que la matriz de varianza-covarianza sea diagonal. Así, solo
# basta utilizar MCO para estimar las ecuaciones estructurales.

#############################
# Estimación MCO automática #
#############################

ofeq <- q ~ pchlag
dmeq <- p ~ q + i

eq_sys   <- list(Oferta = ofeq, Demanda = dmeq)

system_fit <- systemfit(eq_sys, method = "OLS", data = data)

results <- rbind(cbind(t(coef(of)),t(coef(dm))),coef(system_fit))

dimnames(results) <- list(c("MCO 1","MCO 2"), c("Intr_Oferta",
                                                "pchlag_Oferta",
                                                "Intr_Demanda",
                                                "q_Demanda",
                                                "i_Demanda"))

# Resultados finales de la estimación por MCO:

results

# Modelos estimados aproximadamente:

# Oferta:  q_t = 81.98913 + 0.1965089p_{t-1}
# Demanda: p_t = 173.0292 - 1.308515q_t + 0.6052984i_t

###################################
# Conclusión: Análisis matemático #
###################################

# Si se resuelve el sistema de ecuaciones en diferencias, observamos que, si se
# asume que i es constante, lo que no necesariamente es cierto, el valor 
# absoluto de la multiplicación de los coeficientes de pendiente en cada 
# ecuación debe ser menor a uno para establecer la convergencia.

if (abs(results[1,2]*results[1,4]) < 1) {
  cat('Asumiendo "i" constante, el modelo converge.')
} else{
  cat('El modelo no converge.')
}


