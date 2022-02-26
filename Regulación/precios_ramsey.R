############################################################
#                                                          #
# Obtención de los precios Ramsey - Solución segundo mejor #
#                                                          #
# Autor: Sandro Mauricio Peirano Gozalvez                  #
#                                                          #
############################################################

# Este script encuentra los precios Ramsey suponiendo que tanto la función
# de costos como la demanda de cada bien es lineal.

## Funciones del problema de optimización segundo mejor ##

# Función de costos
 
# CT = p1*q1 + p2*q2 - c1*q1 - c2*q2 - Fx

# Demandas
 
# q1 = a - b*p1
# q2 = c - d*p2


## Parámetros de las funciones ##

c1 <- 10
c2 <- 10
Fx <- 150

a <- 80
b <- 1

c <- 100
d <- 2

## Coeficientes del polinomio ##

a0 <- - Fx

a1 <- (a - b*c1)/b + ((c - d*c2)/d)*((c - d*c2)/(a - b*c1))

a2 <- -(1/b + (1/d)*((c - d*c2)/(a - b*c1))^2)

## ¿Soluciones complejas? ##

if (Im(polyroot(c(a0, a1, a2)))[1] < 1e-10) {
  cat("Parte imaginaria de las soluciones igual a cero. Guardando soluciones.")
  q1 <- max(Re(polyroot(c(a0, a1, a2))))
  q2 <- (c-d*c2)/(a-b*c1)*q1
  
  q1_comp <- a - b*c1
  q2_comp <- c - d*c2
  
  p1 <- a/b - 1/b * q1
  p2 <- c/d - 1/d * q2
  
  PES1 <- (a/b - c1)*(q1_comp-q1) - 1/(2*b) * (q1_comp^2-q1^2)
  PES2 <- (c/d - c2)*(q2_comp-q2) - 1/(2*d) * (q2_comp^2-q2^2)
  
  cat("\nGuardado.")
} else{
  cat(paste0("Soluciones con parte imaginaria diferente de cero."))
}

## Resultados finales ##

print_sol <- function(digits){
  cat(paste0("\nResultados del problema de optimización:",
             "\nCantidad Ramsey 1: ",round(q1,digits),
             "\nCantidad Ramsey 2: ",round(q2,digits),
             "\nPrecio Ramsey 1 : ",round(p1,digits),
             "\nPrecio Ramsey 2: ",round(p2,digits),
             "\n\nCantidades primer mejor:",
             "\nCantidad competitiva 1: ",round(q1_comp,digits),
             "\nCantidad competitiva 1: ",round(q2_comp,digits),
             "\n\nPérdida de eficiencia social:",
             "\nPES 1: ",round(PES1,digits),
             "\nPES 2: ",round(PES2,digits)))
}

print_sol(digits = 4)

