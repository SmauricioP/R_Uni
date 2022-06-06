########################################################
#                The Markowitz model                   #
# Minimizing risk or maximizing mean-to-variance ratio #
########################################################

# Loading libraries
library(tidyverse)

# Setting working directory
setwd("Finanzas corporativas")

# Loading data
data <- read_csv("data/hist_acc.csv")
data[,"Date"] = as.Date(data$Date, format = "%m/%d/%Y")
names(data) = tolower(names(data))

stocks <- names(data)[-1]
m <- length(stocks)

# Calculating yields
####################

# We will consider yields as the log of the division since time division is
# small enough.

rtrn <- matrix(NA, nrow = nrow(data) - 1, ncol = m,
               dimnames = list(NULL, stocks))

for (i in 1:m) {
  rtrn[,i] = (log(data[,stocks[i]]/lag(data[,stocks[i]],1)))[-1,]
}

# Participation vector (assuming we start with equal participation)

x <- rep(1/m,m) # Relative participation
W <- 100000     # Total investment
x_W <- W*x      # Money representation

# Sample mean of portfolio return
#################################

rtrn_mean <- apply(rtrn, 2, mean)

port_mean <- as.numeric(t(rtrn_mean) %*% x) # Sample mean of portfolio return

# Correlation and covariance matrix

cor_rtrn <- cor(rtrn)
cov_rtrn <- cov(rtrn)

# Total variance of portfolio
#############################

sd_portfolio <- function(weight, returns){
  cv <- cov(returns)
  vr <- as.numeric(t(weight) %*% cv %*% weight)
  return(sqrt(vr))
}

port_sd <- sd_portfolio(x, rtrn) # Sample standard deviation of portfolio return

sharp <- port_mean/port_sd # Sharp index: quotient between mean and sdev

# Summary
#########

matrix(c(port_mean, port_sd, sharp), 3, 1,
       dimnames = list(c("Mean","Sdev","Sharp"),"Summary"))

rm(list = c("sharp","x", "W", "x_W"))

#############################################
# The Markowitz bullet - No risk-free asset #
#############################################

# The Markowitz problem consist of minimizing total variance of
# portfolio subject to a fixed mean and that the sum of the part-
# ticipations on each asset equals total wealth. Solving the
# problem, and using estimated parameters, we solve for the optimal 
# investment in each stock.

mu <- port_mean # Portfolio fixed mean

# Function for solution vector
mkw_sol <- function(mu, return_data){
  # Covariance matrix of returns
  covmat <- cov(return_data)
  ind_mn <- apply(rtrn, 2, mean)
  n_stck <- length(ind_mn)
  
  # Coefficients for simpler calculation
  a1 <- as.numeric(t(rep(1,n_stck)) %*% solve(covmat) %*% rep(1,n_stck))
  a2 <- as.numeric(t(ind_mn) %*% solve(covmat) %*% rep(1,n_stck))
  a3 <- as.numeric(t(rep(1,n_stck)) %*% solve(covmat) %*% ind_mn)
  a4 <- as.numeric(t(ind_mn) %*% solve(covmat) %*% ind_mn)
  D  <- a1*a4 - a2*a3
  
  # Originally, the lagrange multipliers from the minimization problem
  eta <- (1/D)*a4 - (mu/D)*a2
  lmb <- - (1/D)*a3 + (mu/D)*a1
  
  # Finally, the answer (short position allowed)
  x_opt <- eta*solve(covmat) %*% rep(1,n_stck) + lmb*solve(covmat) %*% ind_mn
  return(x_opt)
}

n <- 400

# Graphing the frontier - grid of values
var_grid  <- NULL
mean_grid <- 0:n/100000
for (i in 0:n) {
  opts <- mkw_sol(i/100000, rtrn)
  var_grid[i+1] <- as.numeric(t(opts) %*% cov_rtrn %*% opts)
  rm("opts")
}

# The Markowitz Bullet - No risk-free assets
ggplot(data = tibble(x = sqrt(var_grid), y = mean_grid)) +
  geom_path(aes(x,y)) +
  xlab("Risk") +
  ylab("Expected return") +
  theme_light() +
  labs(title = "The Markowitz bullet",
       subtitle = "Markowitz frontier for risk assets only")

# Now, for a risk-free asset with return rf, we state that the
# optimum portfolio will consist of a combination of the risky
# portfolio and the risk-free one. This is guaranteed by the
# mutual fund separation theorem.

mkw_sol_norisk <- function(mu, return_data, rf){
  covmat <- cov(return_data)
  ind_mn <- apply(rtrn, 2, mean)
  n_stck <- length(ind_mn)
  
  cof <- ind_mn - rf*rep(1,n_stck)
  num <- mu - rf
  den <- as.numeric(t(cof) %*% solve(covmat) %*% cof)
  
  opt <- (num/den) * (solve(covmat) %*% cof)
  return(opt)
}

rf <- 0.0001 # The risk-free return. Less than portfolio mean-

# Graphing the frontier - grid of values
var_norisk_grid <- NULL
for (i in 0:n) {
  opts <- mkw_sol_norisk(i/100000, rtrn, rf)
  var_norisk_grid[i+1] <- as.numeric(t(opts) %*% cov_rtrn %*% opts)
  rm("opts")
}

# Determining tangency point

rtn_tang <- mean_grid[which.min(var_grid - var_norisk_grid)]
sdv_tang <- sqrt(var_grid[which.min(var_grid - var_norisk_grid)])

# The Markowitz Bullet - No risk-free assets
ggplot(data = tibble(x = sqrt(var_norisk_grid), y = mean_grid)) +
  geom_path(aes(x,y)) +
  xlab("Risk") +
  ylab("Expected return") +
  theme_light() +
  labs(title = "The Markowitz frontier",
       subtitle = "Capital allocation line")

# Graph combining risk-free and risky portfolio efficient frontiers
ggplot() +
  geom_path(data = tibble(x1 = sqrt(var_grid), y1 = mean_grid),
            aes(x1,y1), col = "blue") +
  geom_path(data = tibble(x1 = sqrt(var_norisk_grid), y1 = mean_grid),
            aes(x1,y1), col = "dark green") +
  geom_point(aes(x = sdv_tang,
                 y = rtn_tang),
             fill = "red", shape = 21) +
  geom_text(aes(x = sdv_tang*1.055,
                y = rtn_tang*0.97),
            label = "Tangency point") +
  xlab("Risk") +
  ylab("Expected return") +
  theme_light() +
  labs(title = "The Markowitz frontier",
       subtitle = "Frontiers for risk-free and risk-only situations")

