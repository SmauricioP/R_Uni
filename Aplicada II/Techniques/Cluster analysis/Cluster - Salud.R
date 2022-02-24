library(cluster)
library(factoextra)
library(ggplot2)
library(DataExplorer)
library(funModeling)
library(corrplot)

data = as.data.frame(cdata[,-1])
rownames(data) = cdata$dep
data.e = scale(data) #Estandarización de data

#Análisis exploratorio

summary(data) #Resumen de descriptivos univariados
plot_histogram(data) #Histogramas 
plot_num(data) # Histogramas a colores

d = dist(data.e, method = "euclidean") #Matriz de distancias
fviz_dist(get_dist(data.e , method = "euclidean")) #Visual. matriz

corrplot(as.matrix(d),
         is.corr = FALSE,
         method = "color",
         order = "hclust",
         type = "upper")   #Matriz de distancias

round(as.matrix(d),2)[1:6,1:6]

#Análisis cluster

hres = hclust(d, method = "ward.D2")
hres$merge  #Paso donde se junta
hres$height #Altura ascendente 
rsmn = cbind(c(1:24),hres$merge,hres$height)
colnames(rsmn) = c("Paso","Merge1","Merge2","Altura") ; head(rsmn, n = 10)

fviz_dend(hres, k = 3, cex = 0.8, k_colors = c("red","orange","green"), 
          main = "Dendrograma - disponibilidad de servicios de salud por departamento del Perú en el año 2017",
          ylab = "Altura") +
  geom_hline( color = "#00000F", yintercept = 5, linetype = 2)

fviz_nbclust(data.e, kmeans, method = "wss")

cor(d,cophenetic(hres))

ggplot(cdata,aes(x = "", y = cmed)) +
  geom_boxplot() + 
  geom_point() 

rsmn

