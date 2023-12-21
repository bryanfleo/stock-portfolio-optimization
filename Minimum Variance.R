# Load the package
library(PortfolioAnalytics)
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tidyr)
library(ROI)
library(ROI.plugin.glpk)
library(ROI.plugin.quadprog)

# MINIMUM VARIANCE METHOD
tick <- c('ACES.JK', 'ADRO.JK', 'HRUM.JK', 'INTP.JK', 'ITMG.JK', 'SIDO.JK', 'TINS.JK')
# Download the Price Data
price_data <- tq_get(tick,
                     from = '2021-09-27',
                     to = '2023-09-25',
                     get = 'stock.prices', periodicity='weekly')

# Calculate the weekly returns using logarithmic returns
log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'weekly',
               col_rename = 'ret',
               type = 'log')

# Look at first few rows
head(log_ret_tidy)

# Convert to a wide format and convert into a time series object
log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

# Look at first few rows
head(log_ret_xts)

# Subset the data
index_returns <- log_ret_xts[, c(1:7)]

# Create the portfolio specification
port_spec <- portfolio.spec(colnames(index_returns))

# Add a full investment constraint such that the weights sum to 1
port_spec <- add.constraint(portfolio =port_spec, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
port_spec <- add.constraint(portfolio = port_spec, type = "long_only")

# Objective: Minimize risk
port_rnd <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Objective: Maximize return
port_rnd <- add.objective(portfolio = port_rnd, type = "return", name = "mean")

# 1. Optimise random portfolios
rand_p = optimize.portfolio(R = index_returns, portfolio = port_rnd, optimize_method = "random",
                            trace = TRUE, search_size = 1000)
# plot
chart.RiskReward(rand_p, risk.col = "StdDev", return.col = "mean", chart.assets = TRUE)  #also plots the equally weighted portfolio

# Add an objective to minimize portfolio standard deviation
port_msd <- add.objective(portfolio = port_spec, type = "risk", name = "StdDev")

# Solve the optimization problem
opt <- optimize.portfolio(index_returns, portfolio = port_msd, optimize_method = "ROI")

# Print the results of the optimization
print(opt)


# Calculate the mean weekly returns for each asset.
mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret, 5))

# Calculate the covariance matrix for all these stocks. We will annualized it by multiplying by 52
cov_mat <- cov(log_ret_xts) * 52
print(round(cov_mat,4))

# Weight of the stocs
wts <- c(.1105, .0490, .0456, .3023, .1226, .3284, .0415)
print(wts)

# Calculate the annualized portfolio returns
port_returns <- (sum(wts * mean_ret) + 1)^52 - 1
port_returns

# calculate the portfolio risk (Standard deviation). 
# This will be annualized Standard deviation for the portfolio. 
# We will use linear algebra to calculate our portfolio risk.
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)

# Assume 0% risk free rate to calculate the Sharpe Ratio.
sharpe_ratio <- port_returns/port_risk
print(sharpe_ratio)

#Perhitungan 5 hari kedepan 
saham <- matrix(c('ACES.JK', 'ADRO.JK', 'HRUM.JK', 'INTP.JK', 'ITMG.JK', 'SIDO.JK', 'TINS.JK'))
bobot <- matrix(c(0.1105, 0.0490, 0.0456, 0.3023, 0.1226, 0.3284, 0.0415))
last_price <- matrix(c(750, 2850, 1885, 9950, 28975, 590, 790), nrow=7, ncol=1)
assets<-1000000000000
invest<-bobot/last_price
lembar_saham<-assets*invest
lembar_saham<-data.frame(nama_saham=saham, lembar_saham=lembar_saham)
print(lembar_saham)

saham <- matrix(c('ACES.JK', 'ADRO.JK', 'HRUM.JK', 'INTP.JK', 'ITMG.JK', 'SIDO.JK', 'TINS.JK'))
bobot <- matrix(c(0.1105, 0.0490, 0.0456, 0.3023, 0.1226, 0.3284, 0.0415))
last_price <- matrix(c(785, 2810, 1820, 10100, 28675,585,780), nrow=7, ncol=1)
assets<-1000000000000
invest<-bobot/last_price
lembar_saham<-assets*invest
lembar_saham<-data.frame(nama_saham=saham, lembar_saham=lembar_saham)
print(lembar_saham)

saham <- matrix(c('ACES.JK', 'ADRO.JK', 'HRUM.JK', 'INTP.JK', 'ITMG.JK', 'SIDO.JK', 'TINS.JK'))
bobot <- matrix(c(0.1105, 0.0490, 0.0456, 0.3023, 0.1226, 0.3284, 0.0415))
last_price <- matrix(c(775, 2690, 1735, 10100, 28050,585,785), nrow=7, ncol=1)
assets<-1000000000000
invest<-bobot/last_price
lembar_saham<-assets*invest
lembar_saham<-data.frame(nama_saham=saham, lembar_saham=lembar_saham)
print(lembar_saham)

saham <- matrix(c('ACES.JK', 'ADRO.JK', 'HRUM.JK', 'INTP.JK', 'ITMG.JK', 'SIDO.JK', 'TINS.JK'))
bobot <- matrix(c(0.1105, 0.0490, 0.0456, 0.3023, 0.1226, 0.3284, 0.0415))
last_price <- matrix(c(750, 2660, 1695, 10025, 27200,560,770), nrow=7, ncol=1)
assets<-1000000000000
invest<-bobot/last_price
lembar_saham<-assets*invest
lembar_saham<-data.frame(nama_saham=saham, lembar_saham=lembar_saham)
print(lembar_saham)

saham <- matrix(c('ACES.JK', 'ADRO.JK', 'HRUM.JK', 'INTP.JK', 'ITMG.JK', 'SIDO.JK', 'TINS.JK'))
bobot <- matrix(c(0.1105, 0.0490, 0.0456, 0.3023, 0.1226, 0.3284, 0.0415))
last_price <- matrix(c(760, 2610, 1635, 10125, 26325,605,770), nrow=7, ncol=1)
assets<-1000000000000
invest<-bobot/last_price
lembar_saham<-assets*invest
lembar_saham<-data.frame(nama_saham=saham, lembar_saham=lembar_saham)
print(lembar_saham)
