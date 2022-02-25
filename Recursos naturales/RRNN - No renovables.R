# Matrices para el final de recursos naturales

## Parámetros ##
################

# Nota: DEMANDA DEBE ESTAR P = A-BQ. SINO, TRANSFORMAR

a  <- 4000      # Parámetro de la demanda. También choke price.
b  <- 0.8     # Pendiente de la demanda
c  <- 0.55    # Costo marginal (cmg = cq)
dc <- b+c     # Elementos de la diagonal - competencia perfecta
dm <- 2*b+c   # Elementos de la diagonal - monopolio
t  <- 25       # Número de periodos
r  <- 0.1    # La tasa de interés
R  <- 3500      # Los recursos disponibles


########################
# Competencia perfecta #
########################  

# Se crea la matriz
    
AC <- rbind(cbind(diag(dc,t),(1+r)^seq(0,t-1)),c(rep(1,t),0))

# Finalmente, se obtienen los valores de las cantidades para todos los periodos

qc <- solve(AC,c(a*rep(1,t),R)); round(qc,4)

# Por otro lado, se tienen los precios para cada periodo

pc <- a - b*qc[1:t]; round(pc,4)

#############
# Monopolio #
############# 

# Se crea la matriz

AM <- rbind(cbind(diag(dm,t),(1+r)^seq(0,t-1)),c(rep(1,t),0))

# Finalmente, se obtienen los valores de las cantidades para todos los periodos

qm <- solve(AM,c(a*rep(1,t),R)); round(qm,4)

# Por otro lado, se tienen los precios para cada periodo

pm <- a - b*qm[1:t]; round(pm,4)

###########
# Gráfico #
###########

library(tidyverse)

# Gráfico en cantidades

ggplot(data = tibble(time = 0:(t-1), comp = qc[1:t], monop = qm[1:t])) +
  geom_point(aes(x = time, y = comp, color = "red")) +
  geom_line(aes(x = time, y = comp, color = "red")) +
  geom_point(aes(x = time, y = monop, color = "blue")) +
  geom_line(aes(x = time, y = monop, color = "blue")) +
  xlab("Tiempo") +
  ylab("Cantidades") +
  scale_x_continuous(breaks = seq(0,t-1,1)) +
  scale_color_identity(name = "Senda cantidades",
                       breaks = c("red","blue"),
                       labels = c("Competencia","Monopolio"),
                       guide = "legend") +
  theme_light()

# Gráfico en precios

ggplot(data = tibble(time = 0:(t-1), comp = pc, monop = pm)) +
  geom_point(aes(x = time, y = comp, color = "red")) +
  geom_line(aes(x = time, y = comp, color = "red")) +
  geom_point(aes(x = time, y = monop, color = "blue")) +
  geom_line(aes(x = time, y = monop, color = "blue")) +
  xlab("Tiempo") +
  ylab("Precios") +
  scale_x_continuous(breaks = seq(0,t-1,1)) +
  scale_color_identity(name = "Senda de precios",
                       breaks = c("red","blue"),
                       labels = c("Competencia","Monopolio"),
                       guide = "legend") +
  theme_light()
