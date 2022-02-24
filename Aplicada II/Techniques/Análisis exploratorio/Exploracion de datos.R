# Instalación de Paquetes
library("gmodels")
library("ggplot2")
library("vcd")
library("agricolae")
library("grid")
library("tidyverse")

#########################################################
# Ejemplo: Marketing Directo                          #
#########################################################

# Lectura de datos
DMark <- read.csv("MarketingDirecto.csv")
#DMark<-read.csv(file.choose())
head(DMark)
str(DMark)

#Ordenar niveles de variables categoricas ordinales
DMark$Edad = factor(DMark$Edad,levels = c("Joven","Media","Adulta"),
                    ordered=TRUE)
DMark$Historial = factor(DMark$Historial,levels = c("Bajo","Medio","Alto"),
                    ordered=TRUE)
#DMark$Edad <- ordered(DMark$Edad, levels =  c("Joven","Media","Adulta"))
str(DMark)

#------------------------------------------------------------------#
#  a) Representación de Datos Cualitativos                         #
#------------------------------------------------------------------#

# Tabla de Frecuencia
# -------------------
ni<-table(DMark$Edad)
fi<-prop.table(table(DMark$Edad))
pi<-prop.table(table(DMark$Edad))*100
edad.tabla<-t(rbind(ni,fi,pi))
edad.tabla
table(DMark$Edad)
prop.table(table(DMark$Edad))

## Similar a SAS o SPSS
library(gmodels)
CrossTable(DMark$Edad, format="SAS")
CrossTable(DMark$Edad, format="SPSS")

# Visualización de la Distribución de la Variable
# ------------------------------------------------

# Gráfico de Barras
# -------------------
barplot(pi, main="Distribución de las edades de los clientes", 
        xlab="Grupo Etario", col = c("pink","yellow","green"),
        ylab="Porcentaje de Clientes")

# Usando ggplot2
library(ggplot2)
ggplot(data = DMark, aes(x = Edad, y = ..count.., fill = Edad)) +
  geom_bar() +
  scale_fill_manual(values = c("gray50", "orangered2","black")) +
  labs(title = "Distribución de las edades de los clientes") +
  theme_bw() +
  theme(legend.position = "bottom")
 
help("ggplot")


# Gráfico de Sectores Circulares
# ------------------------------
pie(pi, main="Distribución de la Edad de los Clientes")

## Colocar porcentajes
lbls1 <- paste(names(table(DMark$Edad)), "\n",
               prop.table(table(DMark$Edad))*100,"%", sep="")
pie(pi, labels = lbls1,
	main="Distribución de la Edad de los Clientes")


# Usando ggplot2
df = as.data.frame(fi) ; df #Convertir la tabla a un data frame
pie = ggplot(df, aes(x="", y=Freq, fill=Var1)) + geom_bar(stat="identity", width=1) # Crea una barra simple
pie = pie + coord_polar("y", start=0) + 
      geom_text(aes(label = paste0(round(Freq*100), "%")),
                    position = position_stack(vjust = 0.5)) #Convertir a pie (coordenadas polares)
pie = pie + scale_fill_manual(values=c("#55DDE0", "#F6AE2D", "#F26419", "#999999")) #Escala de colores
pie = pie + labs(x = NULL, y = "% de clientes", 
                           fill = "Grupo Etario",
                          title = "Distribución del Grupo Etario") # Remover y añadir etiquetas/título 
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666")) # Limpiar el formato
pie

# Dot Plots
# ---------
dotchart(prop.table(table(DMark$Edad))*100,
         main="Distribución de la Edad de los Clientes", 
         xlab="% de Clientes")

##___________________________________________________________________________________
## Ejercicio: Analice la distribución del  Historial de Compra del cliente
##___________________________________________________________________________________

#------------------------------------------------------------------#
#  b) Tablas de contingencia                                       #
#------------------------------------------------------------------#
tabla1=table(DMark$Edad,DMark$Historial)
tabla1
#x/y(para cada valor de x, ypredictor de x)
#y/x()
CrossTable(x = DMark$Edad,y = DMark$Historial)

#------------------------------------------------------------------#
#  c) Distribución condicional                                     #
#------------------------------------------------------------------#
tabla2=prop.table(tabla1,margin=1)
tabla2
#graficas, para usar; de barras agrupadas al 100%,graficas de barras componentes al 100%, graficas de mosaico##
# Barras agrupadas
barplot(t(tabla2),col=2:4,beside = T,
        xlab="Grupo Etario",
        ylab="Proporción de Clientes",
        main="Distribución del historial de compra según grupo etario")
legend("topright",legend=levels(DMark$Historial),col=2:4,
       pch=15,title="Historial de Compra")

# Barras Componentes
#si tienen el mismo patron de colores se dirian que son independientes,
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE) #Adicionar espacio extra
barplot(t(tabla2),col=2:4,
        xlab="Grupo Etario",
        ylab="Proporción de Clientes",
        main="Distribución del historial de compra según grupo etario")
legend("topright", inset=c(-0.27,0),legend=levels(DMark$Historial),col=2:4,
       pch=15,title="Historial de Compra")

#Usando ggplot2
ggplot(data = na.omit(DMark), aes(x = Edad, y = ..count.., fill = Historial)) +
  geom_bar(position = "fill") +
  labs(y = "% de clientes", title = "Distribución del historial de compra según grupo etario") +
  theme_bw() +
  theme(legend.position = "bottom")

library(vcd)
mosaicplot(~ Edad+Historial, data = DMark, color = 2:4, 
           main="Distribución del historial de compra según grupo etario")

##___________________________________________________________________________________
## Ejercicio: Analice si existe asociación entre el historial de compra y el género
##___________________________________________________________________________________

#------------------------------------------------------------------#
#  d) Representación de Datos Cuantitativos Discretos              #
#------------------------------------------------------------------#
# Tabla de Frecuencias
ni<-table(DMark$Hijos)
fi<-prop.table(table(DMark$Hijos))
pi<-prop.table(table(DMark$Hijos))*100
hijos.tabla<-t(rbind(ni,fi,pi))
hijos.tabla

# Visualización de la Distribución de la Variable
# ------------------------------------------------
#Gráfico de Varas
plot(pi, type="h", lwd=2,
     xlab="Número de hijos",
     ylab="Porcentaje de clientes",
     main="Distribución del número de hijos por cliente")
points(x =as.numeric(row.names(pi)),
       y =as.numeric(pi),
       pch=19,cex=1.5)
help("plot")
#Usando ggplot2
df <- as.data.frame(pi)
ggplot(df, aes(x=Var1, y=Freq)) +
  geom_segment( aes(x=Var1, xend=Var1, y=0, yend=Freq)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
  labs(x = "# de hijos", y = "% de clientes", title = "Distribución del número de hijos por cliente") 

#------------------------------------------------------------------#
#  e) Representación de Datos Cuantitativos Continuos              #
#------------------------------------------------------------------#

# Tabla de Frecuencias (usando la regla de Sturges)
factorx <- factor(cut(DMark$Monto, breaks=nclass.Sturges(DMark$Monto),right=TRUE))
xout <- as.data.frame(table(factorx))
colnames(xout)<-c("Monto","ni")
xout <- transform(xout, 
                  fi=prop.table(ni),
                  pi=prop.table(ni)*100,
                  Ni = cumsum(ni),
                  Fi = cumsum(prop.table(ni)),
                  Pi = cumsum(prop.table(ni))*100
)
xout

# Obtener la tabla de una librería
library(agricolae)
(table.freq(hist(DMark$Monto,breaks = "Sturges",plot=FALSE))) #Regla Sturges
(table.freq(hist(DMark$Monto,breaks = "Scott",plot=FALSE)))   #Regla de Scott
(table.freq(hist(DMark$Monto,breaks = "FD",plot=FALSE)))      #Regla de Friedman-Diaconis
(table.freq(graph.freq(DMark$Monto,plot=FALSE)))   #Regla Sturges (Agricolae)

# Visualización de la Distribución de la Variable
# ------------------------------------------------
# Histograma y polígono de frecuencia
#clientes con gastos elevados en comparacion al resto#
h1<-hist(DMark$Monto,breaks = "Sturges",
         xlab="Monto",
         ylab="Número de clientes")
polygon.freq(h1,frequency=1,col="red")

# Polígono de Frecuencias (solo)
h1<-hist(DMark$Monto,border=FALSE)
polygon.freq(h1,frequency=1,col="red")

# Usando ggplot2
ggplot(data=DMark, aes(Monto)) + 
  geom_histogram(aes(y =..count..),
                 #col="blue", 
                 fill="black", 
                 alpha = .75) + 
  geom_density(col=2) + 
  labs(title="Monto de Crédito") +
  labs(x="Monto", y="# de clientes")+
  xlim(c(0, 6300)) 

#Histograma y Densidad
hist(DMark$Monto,prob=TRUE)
lines(density(DMark$Monto))
summary(DMark$Monto)
#Gráfico de Densidad
plot(density(DMark$Monto))

# Usando ggplot2
ggplot(data = DMark, aes(x = Monto),fill = Monto) +
  geom_density(color="darkblue", fill="lightblue") +
  geom_rug() +
  theme_bw()


#Boxplots
boxplot(DMark$Monto)

# Usando ggplot2
ggplot(data = DMark, aes(y = Monto)) +
  geom_boxplot(fill="lightblue")+
  theme_bw()

#Ojiva
h<-graph.freq(DMark$Monto,plot=FALSE)
points<-ogive.freq(h,col="red",frame=FALSE)
print(points)
#modelos parametricos cuando los parametros son fintos#################
##___________________________________________________________________________________
## Ejercicio: Analice la distribución del salario de los clientes
##___________________________________________________________________________________

#------------------------------------------------------------------#
#  f) Análisis descriptivo                                         #
#------------------------------------------------------------------#

# Resumen básico
summary(DMark)
summary(DMark$Monto)

# Definir funciones con medidas estadísticas

# Función para calcular CV
CV <- function(x){
  (sd(x)/mean(x))*100
}

# Función para calcular asimetria (Pearson)
A3 <- function(x){
  3*(mean(x)-median(x))/sd(x)
}

# Función para calcular el rango
rango <- function(x){
  diff(range(x))
}

# Función para calcular el rango intercuartílico
RIC <- function(x){
  quantile(x,probs = 0.75,type = 6)-quantile(x,probs = 0.25,type = 6)
}

me<-mean(DMark$Monto)
med<-median(DMark$Monto)
q1<-quantile(x = DMark$Monto,probs = 0.25,type = 6)
q3<-quantile(x = DMark$Monto,probs = 0.75,type = 6)
r<-rango(DMark$Monto)
ric<-RIC(DMark$Monto)
s<-sd(DMark$Monto)
cv<-CV(DMark$Monto)
as3<-A3(DMark$Monto)

resumen<-as.matrix(rbind(me,med,q1,q3,r,ric,s,cv,as3))
colnames(resumen)<-c("Valor")
resumen

# Usando tidyverse
library(tidyverse)
DMark %>% summarise(Media = mean(Monto),
                    Mediana = median(Monto),
                    Q1 = quantile(Monto,probs = 0.25),
                    Q3 = quantile(Monto,probs = 0.75),
                    Min = min(Monto),
                    Max = max(Monto),
                    Rango = rango(Monto),
                    RIC = RIC(Monto),
                    S = sd(Monto),
                    CV = CV(Monto),
                    Asimetria = A3(Monto)
)

# Funciones de resumen de datos disponibles en otras librerías
Hmisc::describe(DMark$Monto)
library(psych)
psych::describe(DMark$Monto)

library(fBasics)
basicStats(DMark$Monto)

skewness(DMark$Monto)
kurtosis(DMark$Monto)

##___________________________________________________________________________________
## Ejercicio: Realice el análisis descriptivo para el salario de los clientes
##___________________________________________________________________________________

#------------------------------------------------------------------#
#  g) Análisis descriptivo comparativo                             #
#------------------------------------------------------------------#

#Usando funciones apply
me<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=mean)
med<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=median)
q1<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=quantile,probs = 0.25,type = 6)
q3<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=quantile,probs = 0.75,type = 6)
r<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=rango)
ric<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=RIC)
s<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=sd)
cv<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=CV)
as3<-tapply(X = DMark$Monto,INDEX = DMark$Ecivil,FUN=A3)

resumen<-as.matrix(rbind(me,med,q1,q3,r,ric,s,cv,as3))
resumen


# Usando librerías
psych::describeBy(x = DMark$Monto, group = DMark$Ecivil)

# Análisis comparativo usando visualización de datos
# ---------------------------------------------------

boxplot(DMark$Monto ~ DMark$Ecivil,
        xlab="Estado Civil",ylab="Gasto",
        main="Comparacion del gasto por estado civil")

# Usando ggplot2
ggplot(data = DMark, aes(x = Ecivil, y = Monto, color = Ecivil)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  theme_bw()

ggplot(data = DMark, aes(x = Monto, fill = Ecivil)) +
  geom_density(alpha = 0.5) +
  geom_rug(aes(color = Ecivil), alpha = 0.5) +
  theme_bw()

##___________________________________________________________________________________
## Ejercicio: Realice un análisis comparativo de los gastos de los clientes por
##___________________________________________________________________________________

#------------------------------------------------------------------#
# h)  Asociación                                                     #
#------------------------------------------------------------------#

plot(DMark$Salario,DMark$Monto)
cor(DMark$Salario,DMark$Monto)
help(cor)
# Matriz de Diagramas de dispersión
pairs(~Salario + Monto + Hijos + Catalogos,data=DMark)
cor(DMark[,c(6,7,9,10)])
corr.test(DMark[,c(6,7,9,10)])
cor.plot(cor(DMark[,c(6,7,9,10)]))
