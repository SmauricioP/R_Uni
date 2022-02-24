library("lattice")
library("ggplot2")
library("factoextra")
library("FactoMineR")
library("psych")
library("ca")
library("ggplot2")

#Prepare data
datos<-as.matrix(corresp1[1:8,2:5],)
rownames(datos) <-as.matrix(corresp1[-9,1])
tab = as.table(datos)
tab #Contingency table

#Column and row profiles (conditionals)
round(prop.table(tab,1),4) #Row profile
round(prop.table(tab,2),4) #Column profile

#Euclidean profile distances
round(get_dist(as.matrix(prop.table(tab,1)), method = "euclidean"),4)#Row
fviz_dist(get_dist(as.matrix(prop.table(tab,1)), method = "euclidean"))

round(get_dist(as.matrix(t(prop.table(tab,2))), method = "euclidean"),4)
fviz_dist(get_dist(as.matrix(t(prop.table(tab,2))), method = "euclidean"))

#Independence test
chisq.test(tab)

#Correspondence analysis
catab = CA(tab)
summary(catab) #Summary of correspondence analysis
round(catab$eig,4) #Eigenvalues and cumulative
fviz_ca_row(catab, repel = TRUE) #Row graph
fviz_ca_col(catab, reoel = TRUE) #Column graph
fviz_ca_biplot(catab, repel = TRUE, title = "Categoría - Relevancia") #Col&row graph

#Euclidean profile distances (with marginals)
rowprof=rbind(prop.table(tab,1),catab$call$marge.col)
colprof=cbind(prop.table(tab,2),catab$call$marge.row)
round(get_dist(as.matrix(rowprof), method = "euclidean"),4)
fviz_dist(get_dist(as.matrix(prop.table(tab,1)), method = "euclidean"))
round(get_dist(as.matrix(t(colprof)), method = "euclidean"),4)
fviz_dist(get_dist(as.matrix(t(prop.table(tab,2))), method = "euclidean"))

