library(car)
library(tidyverse)
library(nortest)
library(corrplot)

setwd("C:/Users/Mauca/Desktop/R/Data") # Ruta de la base de datos
data <- read.csv("regdata.csv")

#Y : W
#X1: RS
#X2: RA
#X3: G

#######################
#Análisis exploratorio#
#######################

summary(data)

ggplot(data, aes(Y)) +
  geom_histogram(binwidth = 6, color = "black", fill = "green") + #Histogram
  labs(title = "Histograma de partidas ganadas") +
  xlab("Y (Número de partidas ganadas)") +
  ylab("Frecuencia") +
  theme_bw()

ggplot(data, aes(Y, y = ..density..)) +
  geom_density(color = "black", fill = "red", alpha = 1/4) +
  labs(title = "Distribución - partidas ganadas") +
  xlab("Y (Número de partidas ganadas)") +
  ylab("Densidad") +
  theme_bw()

ggplot(data, aes(sample = Y)) +   #Diagrama qq para normalidad
  geom_qq_line(distribution = stats::qnorm, color = "red") +
  geom_qq(alpha = 0.125) +
  labs(title = "Gráfico Q-Q - partidas ganadas") +
  xlab("Cuantiles teóricos") +
  ylab("Cuantioes observados") +
  theme_bw()

ggplot(data,aes(X1,Y)) +
  geom_point(alpha = 1/2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
  labs(title = "Diagrama de dispersión - partidas ganadas vs. runs anotados ") +
  xlab("X1 (Runs anotados)") +
  ylab("Y (Número de partidas ganadas)") +
  theme_bw()

ggplot(data,aes(X2,Y)) +
  geom_point(alpha = 1/2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
  labs(title = "Diagrama de dispersión - partidas ganadas vs. runs permitidos ") +
  xlab("X2 (Runs permitidos)") +
  ylab("Y (Número de partidas ganadas)") +
  theme_bw()

ggplot(data,aes(X3,Y)) +
  geom_point(alpha = 1/2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
  labs(title = "Diagrama de dispersión - partidas ganadas vs. partidas jugadas ") +
  xlab("X3 (Partidas jugadas)") +
  ylab("Y (Número de partidas ganadas)") +
  theme_bw()

round(cor(data[,2:4]),4)

corrplot(cor(data[,2:4]))

###########################
#Regresión lineal múltiple#
###########################

lmod <- lm(Y ~ ., data) #Modelo lineal

summary(lmod) #Resumen del modelo

lmod$coefficients #Solo coeficientes
lmod$fitted.values #Valores estimados medios (por observación)

predict(lmod, data.frame(X1 = 73, X2 = 85, X3 = 159)) #Predicción puntual

predict(lmod, data.frame(X1 = 73, X2 = 85, X3 = 159), #intervalo de confianza
        interval = "confidence",
        level = 0.95)

vif(lmod) #Factor de inflación de la varianza

#Validación de hipótesis

ad.test(lmod$residuals) #Test Anderson-Darling normalidad (cuidado muestras grandes).
sf.test(lmod$residuals) #Test Shapiro-Francia normalidad (cuidado muestras grandes).

ncvTest(lmod) #Homocedasticidad. H0: varianza del error constante.

durbinWatsonTest(lmod) #Independencia de errores. H0: Independencia.

plot(lmod)

####################
#Regresión (manual)#
####################

#Importar datos (PRIMERA COLUMNA: Y)

(X <- as.matrix(cbind(Y = rep(1,nrow(data)),data[,2:ncol(data)]))) #Matriz regresores

(Y <-  as.matrix(data[,1])) #Vector de observaciones

(y. <- sum(data$Y)) #Suma de las observaciones

(n <- nrow(data)) #Número de observaciones

(q <- diag(solve(t(X) %*% X), names = FALSE)) #Diag. de matriz inversa

#Encontrar beta = [X'X]^-1 X'y

b <-  solve(t(X) %*% X) %*% t(X) %*% Y #Vector de coeficientes

k <- nrow(b) - 1 #Grados de libertad - regresión

colnames(b) = c("Coeficientes") ; rownames(b) =c(paste0("b",0:k)); b

#Varianzas

(screg <- as.numeric(t(b) %*% t(X) %*% Y - (y.)^2/n)) #S.C. regresión
(sct <- as.numeric(t(Y) %*% Y - (y.)^2/n)) #S.C total
(sce <- sct - screg) #S.C error

(f <- (screg/k)/(sce/(n-k-1))) #f-snedecor

(cme <- (sce/(n-k-1))) #C. medios

t <-  b/(sqrt(cme*q)); colnames(t) = c("tcal"); t #t-student

#Resumen

rest <- as.data.frame(cbind(round(b,5),round(t,5),
                            tcrit = tcrit <- round(rep(qt(0.975,n-k-1),nrow(b)),5),
                            sign = ifelse(between(t,-tcrit,tcrit),
                                          "no rechaza","rechaza")))

resf <- as.data.frame(cbind(var = c("regresión","residual","total"),
              s.cuad = round(c(screg, sct, sce),2),
              g.lib  = c(k, n - k - 1, n - 1),
              c.med  = c(round(c(screg/k, sce/(n-k-1)),2),""),
              fcal   = c(round(f,2),"",""),
              fcrit  = c(round(qf(0.95, k, n-k-1),2),"",""),
              sign   = c(ifelse(f > qf(0.95, k, n-k-1),"rechaza","no rechaza"),
                       "",""),
              p      = c(1 - pf(26.52, k, n - k - 1),"","")))

rest #Tabla para hipótesis t-Student

resf #Tabla para hipótesis f-Snedecor

(r <- screg/sct) #R cuadrado
(radj <- 1-((n - 1)/(n - k - 1))*(1-r)) #R cuadrado ajustado

