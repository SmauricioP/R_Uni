#########################################################################
#                                                                       #
# Obtención de la función de costos de cada empresa - Monopolio Natural #
#                                                                       #
# Autor: Sandro Mauricio Peirano Gozalvez                               #
#                                                                       #
#########################################################################

# Este script encuentra los parámetros de las funciones de costos cuando están
# en la misma industria. Recordar que estas funciones de costo son resultado de
# el problema de minimización de costos de la industria. 

res <- function(params){
  n = length(params)
  l = rep(1,n)
  A = diag(params)
  return(as.numeric(1/(t(l) %*% solve(A) %*% l)) * (solve(A) %*% l))
}

tot_cost <- function(fixed, params){
  n = length(params)
  l = rep(1,n)
  A = diag(params)
  q = res(params)
  return(c(sum(fixed),as.numeric(t(q) %*% A %*% q)))
}

##############
# Resultados #
##############

## Costos fijos

fix <- c(16,16)

## Costos variables

params <- c(1,1.5)

# Proporciones de la producción
###############################

res(params)

# Coeficientes del costo total
##############################

tot_cost(fix,params)


