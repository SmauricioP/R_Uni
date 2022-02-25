
library(tidyverse)
library(mvnormalTest)

# Parameters for the monte-carlo experiment

m <- 1000
n <- 1000
p <- 4

set.seed(1)
b0 <- round(runif(1,-50,50),0)
beta <- round(runif(p,-10,20),0)
y <- b0

beta_store <- matrix(0,m,p+1, dimnames = list(NULL,sprintf("b%s",seq(0,p))))


for (k in 1:p) {
  assign(paste0("x",k), runif(n,-10,10))
  y = y + beta[k]*eval(parse(text = paste0("x",k)))
}

# Prepare dataframe

data <- data.frame(y = y)
for (i in 1:p) {
  data[,i+1] = eval(parse(text = paste0("x",i)))
}
colnames(data) = c("y",sprintf("x%s",seq(1,p)))

# Begin experiment

for (i in 1:m) {
  e = rnorm(n)
  y = y + e
  data$y = y
  mod <- lm(y~., data = data)
  coefi = as.numeric(coef(mod))
  beta_store[i,] = coefi
  y = y-e
}

# Means of betas
beta_means <- apply(beta_store,2,mean)

# Densities of some estimators

ggplot(data = data.frame(b0 = beta_store[,1]), aes(x = b0)) +
  geom_density(fill = "red", alpha = 0.3) +
  geom_vline(xintercept = beta_means[1], linetype = 2) +
  xlab("b0") +
  ylab("Density") +
  labs(title = "Intercept density") +
  theme_light()

ggplot(data = data.frame(b1 = beta_store[,2]), aes(x = b1)) +
  geom_density(fill = "green", alpha = 0.3) +
  geom_vline(xintercept = beta_means[2], linetype = 2) +
  xlab("b1") +
  ylab("Density") +
  labs(title = "b1 density") +
  theme_light()

ggplot(data = data.frame(b2 = beta_store[,3]), aes(x = b2)) +
  geom_density(fill = "blue", alpha = 0.3) +
  geom_vline(xintercept = beta_means[3], linetype = 2) +
  xlab("b2") +
  ylab("Density") +
  labs(title = "b2 density") +
  theme_light()

# Are betas multivariate normal?

set.seed(420)
faTest(beta_store[sample(nrow(beta_store),500),], B = 350) 

mardia(beta_store)
