####################################
# Modelo de ecuaciones simultáneas #
####################################

# Autor: Sandro Mauricio Peirano Gozalvez

# Se pretende estimar los parámetros de modelos de ecuaciones simultáneas
# utilizando como marco teórico el libro de Pichihua.

##############################################################################
# Kmenta - Descripción de la data                                            #
#                                                                            #
# Series de tiempo anuales de cinco variables macroeconómicas:               #
#                                                                            #
# consump   : Consumo de alimento per cápita                                 #
# price     : Ratio de precios de alimento a precios generales de consumo    #
# income    : Ingresos en dólares constantes                                 #
# farmPrice : Ratio de precios de años anteriores recibidos por granjeros    #
# trend     : Tendencia de tiempo en años                                    #
##############################################################################

## Working directory ##

setwd("Econ. interm/")

## Librerías ##

library(ivreg)     # Regresión por MC2E
library(tidyverse) # Siempre es bueno tenerlo
library(haven)     # Abrir archivos stata
library(systemfit) # Estimación MES
library(nlme)      # Estimación por MCG

## Data ##

data("Kmenta")

############################################################################

## Especificación ##

# Se estimará un modelo simple de oferta y demanda para demostrar cómo funciona
# la estimación de parámetros en modelos de ecuaciones simultáneas.

###########################################
# Regresiones:                  
# consump ~ price + income    
# consump ~ price + farmPrice + trend     
###########################################

# Variables endógenas: C, P
# Variables exógenas: income, farmPrice, trend

# Realmente, acá está presente una identidad: Q_d = Q_s, es decir, el equilibrio
# de la oferta y de la demanda.

## Fórmulas del MES ##

dmd <- consump ~ price + income             # Ecuación de demanda
oft  <- consump ~ price + farmPrice + trend # Ecuación de oferta

inst <- ~ income + farmPrice + trend # Las instrumentales o exógenas

system <- list(Demanda = dmd, Oferta = oft) # Lista con el sistema

# Identificación #
##################

# Se puede observar que, mientras que la ecuación de oferta está exactamente 
# identificada, la ecuación de demanda está sobreidentificada, pues hay dos
# variables exógenas excluídas en la primera ecuación.

# Si queremos utilizar un método de estimación limitada (Pichihua, p. 240), se
# utilizarán MC2E. Si se desea utilizar un método de información completa, uno
# deberá optar por MC3E (Pichihua, p. 250).

################################################################################

# Estimación #
##############

# Información limitada - MC2E #
###############################

## Etapa I ##

# Estimación de los "fitted" para ser utilizados como instrumentos. Se utiliza 
# la forma reducida de las ecuaciones.

red1 <- lm(consump ~ income + farmPrice + trend, Kmenta)
red2 <- lm(price ~ income + farmPrice + trend, Kmenta)

# Se extraen los valores predichos para utilizarse como instrumentos:

Kmenta[,"cons_fit"] = as.numeric(red1$fitted.values)
Kmenta[,"price_fit"] = as.numeric(red2$fitted.values)

## Etapa II ##

# Estimación por separado de cada ecuación, reemplazando las endógenas por los
# valores "fitted".

fit1_mc2e <- lm(consump ~ price_fit + income, Kmenta)
fit2_mc2e <- lm(consump ~ price_fit + farmPrice + trend, Kmenta)

# Nota: Como se estima cada ecuación como si estuviera sola, este tipo de 
# estimación recibe el nombre de estimación de información limitada.

# Se puede automatizar todo el proceso de la siguiente manera:

fit_mc2e <- systemfit(system, method = "2SLS", inst = inst, data = Kmenta)

# Información completa - MC3E #
###############################

## Etapa I ##

# Estimación de los "fitted" para ser utilizados como instrumentos. Se utiliza 
# la forma reducida de las ecuaciones.

  # Esto ya fue hecho con MC2E

## Etapa II ##

# Estimación por separado de cada ecuación, reemplazando las endógenas por los
# valores "fitted". Ahora, se utilizan los residuales para estimar la matriz de
# varianza-covarianza de los residuales.

res1 <- as.numeric(fit1_mc2e$residuals)
res2 <- as.numeric(fit2_mc2e$residuals)

sigma <- fit_mc2e$residCov # La matriz de covarianza de las dos etapas. 
sg_krk_id <- kronecker(solve(sigma), diag(20))

## Etapa III ##

# Se estima mediante SUR. Esta estimación es básicamente un MCGF grande, en el
# sentido de que la matriz de varianza-covarianza toma una forma específica.

X1 <- model.matrix(~ price_fit + income, Kmenta)
X2 <- model.matrix(~ price_fit + farmPrice + trend, Kmenta)

zero1 <- matrix(0,20,4)
zero2 <- matrix(0,20,3)

X <- rbind(matrix(c(X1,zero1),20,7),matrix(c(zero2,X2),20,7))
y <- matrix(rep(Kmenta$consump,2))

beta_mc3e <- solve(t(X) %*% sg_krk_id %*% X) %*% (t(X) %*% sg_krk_id %*% y)
dimnames(beta_mc3e) <- list(names(coef(fit_mc2e)),"")

coef(fit_mc3e)

# Automatizando todo el proceso:

fit_mc3e <- systemfit(system, method = "3SLS", inst = inst, data = Kmenta)

t(beta_mc3e)

