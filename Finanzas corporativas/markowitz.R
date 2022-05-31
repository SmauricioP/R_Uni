########################################################
#                The Markowitz model                   #
# Minimizing risk or maximizing mean-to-variance ratio #
########################################################

# Loading libraries
library(tidyverse)
library(nloptr)

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

w <- rep(1/m,m) # Relative participation
S <- 100000     # Total investment
w_S <- S*w      # Money representation

# Sample mean of portfolio return
#################################

rtrn_mean <- apply(rtrn, 2, mean)

port_mean <- as.numeric(t(rtrn_mean) %*% w) # Sample mean of portfolio return

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

port_sd <- sd_portfolio(w, rtrn) # Sample standard deviation of portfolio return

sharp <- port_mean/port_sd # Sharp index: quotient between mean and sdev

# Summary
#########

matrix(c(port_mean, port_sd, sharp), 3, 1,
       dimnames = list(c("Mean","Sdev","Sharp"),"Summary"))


########################################
# Minimizing risk by assigning weigths #
########################################


