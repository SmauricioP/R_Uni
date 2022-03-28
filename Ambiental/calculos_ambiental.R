####################################
# Fórmulas para economía ambiental #
####################################


# 1.- P y Q de equilibrio con excedentes, mcdo. competitivo.
############################################################

# IMPORTANTE: Se asumen ecuaciones inversas de la forma 
# p = a + bq (Supply)
# p = c - dq (Demand)
#
# De no ser así, inverse = FALSE
# Nota: Todos los parámetros deben ser positivos.

a <- 20
b <- 0.75 + 0.08
c <- 50
d <- 0.135

q_eq <- function(a, b, c, d, inverse = TRUE){
  if (inverse == TRUE) {
    return((c-a)/(b+d))
  }
  else{
    return((a*d + b*c)/(b + d))
  }
}
p_eq <- function(a, b, c, d, inverse = TRUE){
  if (inverse == TRUE) {
    return((a*d + b*c)/(b + d))
  }
  else{
    return((c-a)/(b+d)) 
  }
}

# Excedente del productor solo funciona si la curva no empieza desde eje q.

surpl_cons <- function(a, b, c, d, inverse = TRUE){
  if (inverse == TRUE) {
    p_e = p_eq(a, b, c, d)
    q_e = q_eq(a, b, c, d)
    return((c-p_e)*q_e - (d/2)* q_e^2)
  }
  else{
    a_new = -a/b
    b_new =  1/b
    c_new =  c/d
    d_new =  1/d
    p_e = p_eq(a_new, b_new, c_new, d_new)
    q_e = q_eq(a_new, b_new, c_new, d_new)
    return((c_new-p_e)*q_e - (d_new/2)* q_e^2)
  }
}
surpl_sppl <- function(a, b, c, d, inverse = TRUE){
  if (inverse == TRUE) {
    p_e = p_eq(a, b, c, d)
    q_e = q_eq(a, b, c, d)
    return((p_e-a)*q_e - (b/2)* q_e^2)
  }
  else{
    a_new = -a/b
    b_new =  1/b
    c_new =  c/d
    d_new =  1/d
    p_e = p_eq(a_new, b_new, c_new, d_new)
    q_e = q_eq(a_new, b_new, c_new, d_new)
    return((p_e-a_new)*q_e - (b_new/2)* q_e^2)
  }
}
result_1 <- function(a, b, c, d, inverse = TRUE, digits = 2){
  if (inverse == TRUE) {
    q <- q_eq(a,b,c,d)
    p <- p_eq(a,b,c,d)
    ex_cons <- surpl_cons(a,b,c,d)
    ex_prod <- surpl_sppl(a,b,c,d)
    tot_ex <- ex_cons + ex_prod
  }
  else{
    q <- q_eq(a,b,c,d, inverse = FALSE)
    p <- p_eq(a,b,c,d, inverse = FALSE)
    ex_cons <- surpl_cons(a,b,c,d, inverse = FALSE)
    ex_prod <- surpl_sppl(a,b,c,d, inverse = FALSE)
    tot_ex <- ex_cons + ex_prod
  }
  cat(paste0("\nCantidad de equilibrio: ",round(q,digits),
             "\nPrecio de equilibrio: ",round(p,digits),
             "\nExcedente consumidor: ",round(ex_cons,digits),
             "\nExcedente productor: ",round(ex_prod,digits),
             "\nExcedente total: ",round(tot_ex,digits)))
}


result_1(a, b, c, d, inverse = TRUE, 2) # Resultado total


# 2.- Costo de abatimiento y óptimo social
##########################################

# Se asumen las siguientes funciones
# bt = aq - bq^2 + fc
# ct = cq + dq^2 + fb

##########################################################################

## Asignación óptima ##

a <- 200
b <- 0.7
c <- 0
d <- 0.5

fc <- 0
fb <- 0

bt <- function(a, b, fb, q){
  return(a*q - b*q^2 + fb)
}
ct <- function(c, d, fc, q){
  return(c*q + d*q^2 + fc)
}

q_opt <- function(a,b,c,d){
  return((a-c)/(2*(b+d)))
}

result_2.1 <- function(a, b, c, d, fb, fc, digits = 2){
  q <- q_opt(a,b,c,d)
  benf <- bt(a,b,fb,q)
  cost <- ct(c,d,fc,q)
  cat(paste0("Cantidad óptima: ",round(q,digits),
             "\nBeneficio total: ",round(benf,digits),
             "\nCosto total: ",round(cost,digits)))
}

result_2.1(a,b,c,d,fb,fc, digits = 3)

##########################################################################

## Varias empresas con misma asignación anterior ##

# Se asume que el resto de empresas tienen funciones de costo del tipo:
# CT_i = CF_i + C_i,1q + Ci_2q^2

sol_no_opt <- function(a, b, c, d, cost){
  q = q_opt(a,b,c,d)
  n = length(cost)
  
  A = diag(n)
  for (i in 1:n) {
    A[i,i] = -2*cost[[i]][3] 
  }
  
  A = rbind(A,c(rep(1,n)))
  A = cbind(A,c(rep(-1,n),0))
  
  b = rep(0,n)
  for (i in 1:n) {
    b[i] = cost[[i]][2]
  }
  b[n+1] = q
  
  sol = solve(A,b)
  return(sol)
}
sol_opt <- function(a, b, c, d, cost){
  n = length(cost)
  
  A = diag(n+1)
  for (i in 1:n) {
    A[i,i] = -2*cost[[i]][3] 
  }
  A[n+1,n+1] = -2*b
  A = cbind(A,c(rep(-1,n),1))
  A = rbind(A,c(rep(1,n),-1,0))
  
  b = rep(0,n+2)
  for (i in 1:n) {
    b[i] = cost[[i]][2]
  }
  b[n+1] = - a
  
  sol = solve(A,b)
  return(sol)
}
result_2.2 <- function(a,b,c,d, costos, digits = 2){
  n = length(costos)
  q = q_opt(a,b,c,d)
  noopt = sol_no_opt(a,b,c,d, costos)
  opt   = sol_opt(a,b,c,d, costos)
  cat(paste0("\nSi se parte con la asignación inicial de q = ",
             round(q, digits),":\n"))
  for (i in 1:n) {
    cat(paste0("Descontaminación de la empresa ",i,": ", round(noopt[i],digits),"\n"))
  }
  
  cat("\nAsignación costo efectiva: q = ",round(opt,digits)[n+1],
      "\nLas empresas se reparten de la siguiente manera:\n\n")
  for (i in 1:n) {
    cat(paste0("Descontaminación de la empresa ",i,": ", round(opt[i],digits),"\n"))
  }
}

costos <- list(c(0,0,1),
               c(0,0,2),
               c(0,0,6))

result_2.2(a,b,c,d,costos, 3)

