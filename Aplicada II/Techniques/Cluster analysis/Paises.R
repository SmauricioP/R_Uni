library(cluster)
library(factoextra)
library(ggplot2)
library(DataExplorer)
library(funModeling)
library(corrplot)

data = as.data.frame(dataval[,-1]) #Importar tus datos de excel
rownames(data) = dataval$pais #Asigna nombres a cada fila
data.e = scale(data) #¡Importante! Estandariza datos

#Análisis exploratorio

summary #Resumen de descriptivos univariados
plot_histogram(data) #Sin colores
plot_num(data) #Con colores

d = dist(data.e, method = "euclidean") ; round(as.matrix(d)[1:7,1:7],3) #Matriz de distancia
fviz_dist(get_dist(data.e , method = "euclidean"), 
          gradient = list(low ="black",mid ="grey",high="green")) #Visualizar matriz

corrplot(as.matrix(d),
         is.corr = FALSE,
         method = "color",
         order = "hclust",
         type = "upper") #Visualización triángulo 

#Análisis cluster

hres = hclust(d, method = "ward.D2") #Análisis cluster en sí
hres$merge  #Paso donde se junta
hres$height #Altura ascendente 
rsmn = cbind(hres$height,hres$merge)
colnames(rsmn) = c("Altura","Merge1","Merge2") ; head(rsmn, n = 10)

fviz_dend(hres, k = 3, cex = 0.8, k_colors = c("#c39797","#42a7a4","#8b8378","#35354f"), 
          main = "Título por confirmar",
          ylab = "Altura") +
  geom_hline( color = "#078DE1", yintercept = 9, linetype = 2)

cor(d,cophenetic(hres)) #Número cercano a 1 entonces clasificación más exacta

########################################################################

fviz_nbclust(data.e, kmeans, method = "wss") #Número de cluster - método elbow

km = kmeans(data,
            centers = 3,    #Número clusters
            iter.max = 100, #Iteraciones 
            nstart = 25)    #Puntos iniciales

fviz_cluster(km, data, repel = TRUE)
clusplot(data, km$cluster, lines = 0)

