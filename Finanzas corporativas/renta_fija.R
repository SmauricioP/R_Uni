
######################################
# Instrumentos de renta fija - bonos #
######################################

# Working directory

setwd("Finanzas corporativas")

# Librerías útiles

library(tidyverse)
library(scales)

# Definición de funciones importantes

# VNA: Valor neto actual de una corriente de flujos. En el 1er flujo
# ya pasó un periodo.
vna <- function(coup, int){
  pres = 0
  tm = length(coup)
  for (k in 1:tm) {
    pres = pres + coup[k]/((1+int)^k)
  }
  return(pres)
}

# Valuación de un bono en base a la tasa de mercado vs. tasa cupón
bond_price <- function(vn, tm, i, r){
  cp <- rep(vn*i, tm)
  return(vna(cp, r) + vn/(1+r)^tm)
}

# Valores

i <- 0.1
r <- 0.1
t <- 10
vn <- 10000

bond_price(vn, t, i, r)

# Gráfico - valoración de bonos respecto a distintas tasas de mercado
#####################################################################

# Se observa cuál es la sensibilidad de los bonos ante la variación de tasas
# de mercado dependiendo del plazo del bono.

r_grid <- seq(0,0.2, by = 0.01)
t_grid <- c(1,10,30, 100000)

price_grid <- matrix(0, length(r_grid), length(t_grid)) %>% 
  cbind(r_grid) %>% as_tibble()

for (n in 1:length(t_grid)) {
  for (k in 1:length(r_grid)) {
    price_grid[k,n] <- bond_price(vn, t_grid[n], i, r_grid[k])
  }
}

ggplot(price_grid, aes(x = r_grid)) +
  geom_path(aes(y = V1, color = "red")) +
  geom_path(aes(y = V2, color = "yellow")) +
  geom_path(aes(y = V3, color = "green")) +
  geom_path(aes(y = V4, color = "black")) +
  geom_vline(xintercept = 0.10, linetype = 2) +
  geom_hline(yintercept = vn, linetype = 2) +
  geom_text(aes(x = 0.06, y = 35000), label = "Bonos con prima", cex = 4.5) +
  geom_text(aes(x = 0.15, y = 30000), label = "Bonos con descuento", cex = 4.5) +
  coord_cartesian(ylim = c(0,45000)) + 
  scale_y_continuous(breaks = seq(0,60000,10000), labels = comma,
                     expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_color_identity(name = "Plazos",
                       breaks = c("red", "yellow", "green", "black"),
                       labels = c("1 año", "10 años", "30 años", "Perpetuo"),
                       guide = "legend") +
  xlab("Tasas de interés del mercado") +
  ylab("Precio del bono") +
  labs(title = paste0("Valoración de los bonos respecto a las",
                      " tasas de interés del mercado"),
       subtitle = "Tasa de cupón: diez por ciento") +
  theme_light()

rm(list = ls())

################################################################################

# Aplicación práctica: estimación de la curva de rendimiento

# Se estimará la curva de rendimiento utilizando la información disponible en
# la página sbs.gob.pe. Así, se realizará una regresión de la TIR esperada sobre
# el plazo de vencimiento.


# Lectura de los datos:
df <- read_csv("data/hist_bonos.csv", locale=locale(encoding="latin1"))
names(df)

# Ahora, se seleccionan solo aquellos bonos emitidos por el gobierno y cuyo 
# origen sea del mercado. Además, solo se guardarán algunas variables.

# Se elegirá el día de inicio de la fecha y se asumirá un mismo día de
# emisión de bonos de tal manera de que todos los plazos anuales sean positivos.

fecha_ini <- "9/05/2022"

df <- df %>% 
  filter(`Origen(*)` == "Mercado" & Emisor == "GOB.CENTRAL") %>%
  select(tir = `TIR %`, p_limp = `P. Limpio (monto)`, p_suc = `P. Sucio (monto)`,
         f_venc = `F. Vencimiento`, coup = `Cupón`) %>% 
  mutate(
    plazo_d = as.numeric(
      difftime(as.Date(f_venc, format = "%d/%m/%Y"), 
               as.Date(fecha_ini, format = "%d/%m/%Y"),
               units = "days")),
    plazo_y = plazo_d/360) %>% 
  filter(plazo_d > 0) %>% 
  arrange(plazo_y)

# Como se hizo en clase, se estima un polinomio de orden cuatro como curva de
# rendimiento.

y_curve <- lm(tir ~ poly(plazo_y, degree = 4, raw = TRUE),
              data = df)

tir_grid <- predict(y_curve, list(plazo_y = 0:380/10))

dummy_df <- tibble(plazo_y = 0:380/10, tir = tir_grid)

ggplot(df) +
  geom_point(aes(x = plazo_y, y = tir), alpha = 0.25, shape = 21) +
  geom_path(data = dummy_df, mapping = aes(x = plazo_y, y = tir),
            color = "blue") +
  scale_x_continuous(breaks = seq(0,35,5)) +
  scale_y_continuous(breaks = seq(6,8.5,0.25)) +
  xlab("Plazo del bono (años)") +
  ylab("TIR esperada") +
  labs(title = "Curva de rendimiento estimada",
       subtitle = paste0("Fecha de emisión inicial: ", fecha_ini)) +
  theme_light()

