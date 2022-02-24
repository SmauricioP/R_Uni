library(tidyverse)
library(corrplot)
library(ResourceSelection)
library(ROCR)
library(caret)

#Prepare data

setwd("C:/Users/Mauca/Desktop/R/Data")
data <- read.csv("dataR2.csv")
data$Classification = data$Classification - 1

summary(data)

#First model

logistic1 <- glm(Classification ~ Glucose, family = "binomial", data) #Model
summary(logistic1) #Summary of model

df1 <- data.frame(data, prob = logistic1$fitted.values)

ggplot(df1) +
  geom_point(aes(x =Glucose, Classification), color = "red", size = 1.5) +
  geom_point(aes(x =Glucose, prob), color ="blue", size = 1.5) +
  geom_segment(aes(x= Glucose, y = Classification,
                   xend = Glucose, yend = prob), linetype = 2,
               color = "black", alpha = 1/4) +
  geom_smooth(aes(x =Glucose, y = Classification), method = "glm",
              method.args = list(family = "binomial"), formula = y ~ x, se = F,
              color = "black", size = 1, linetype = 2) +
  theme_bw()

result1 <- ifelse(df1$prob > 0.9,1,0)
confusionMatrix(factor(result1, levels = c(0,1)),factor(data$Classification, levels = c(0,1)))
(acc1 <- mean(ifelse(result1 == df1$Classification, 1,0)))*100 #Accuracy

z <- predict(logistic1, data.frame(Glucose = 260))
1/(1+exp(-z))


#Second model (multiple variables)


logistic2 <- glm(Classification ~ BMI + Glucose + Resistin, family = "binomial", data)
summary(logistic2)

df2 <- data.frame(data,prob = logistic2$fitted.values)
head(df2)

result2 <- ifelse(df2$prob > 0.5,1,0)
(acc2 <- mean(ifelse(result2 == df2$Classification, 1,0)))*100 #Accuracy

hoslem.test(logistic1$y,logistic1$fitted.values, g = 10)
hoslem.test(logistic2$y,logistic2$fitted.values, g = 30)

#https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Coimbra



