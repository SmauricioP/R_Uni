library("lattice")
library("FactoMineR")
library("psych")
library("nFactors")

setwd("C:/Users/Mauca/Desktop/R/Data")

data <- Ingresantes[,c(2:10)]
cmdat <- cor(data)
round(cmdat,4) #Correlation matrix up to 4 decimals
summary(data) #Some descriptives

#Bartlett sphericity test and KMO
cortest.bartlett(cmdat, n = 541)
print(KMO(cmdat), digits = 4)


#Number of factors to extract
ev <- eigen(cmdat)
nS <- nScree (x=ev$values)
plotnScree(nS) #Plots Scree graph
nS$Analysis #Shows Eigenvalues and cumulative.

#Extracting components
pcdat <- principal(data, nfactors = 3, rotate ="varimax" , scores = TRUE)
names(pcdat) 
round(pcdat$communality, 3) #Shows communalities
pcdat$loadings #Shows rotated component matrix
round(pcdat$scores[c(1:15),],4) #Shows factor scores

