library(tidyquant)
library(dplyr)
library(tseries)
library(tidyr)
library(cli)
library(ggplot2)
library(readxl)

# Daftar Saham
saham <- c("ACES.JK", "ADRO.JK", "HRUM.JK", "INTP.JK", "ITMG.JK", "SIDO.JK", "TINS.JK")

# Tanggal Awal dan Akhir Periode
start_date <- as.Date("2021-09-27")
end_date <- as.Date("2023-09-27")

path_to_excel <- "C:/Users/ASUS/Downloads/Data UTS.xlsx"
data <- read_excel(path_to_excel, sheet = "Close Price Saham Terpilih")
data <- data.frame(data)
data

returns <- sapply(select(data, -Date)
                  ,FUN = function(x) diff(log(x), lag = 1))
returns

mean_returns <- (colMeans(returns) + 1)^252 - 1
cov_returns <- cov(returns) * 252
sd_returns <- diag(cov_returns) %>% sqrt()
rf <- 0.02

num_of_portfolios <- 1e4
weights <- matrix(nrow = num_of_portfolios, ncol = ncol(returns))

portfolio_metrics <- data.frame(
  returns = rep(0, num_of_portfolios)
  ,risk = rep(0, num_of_portfolios)
  ,sharpe = rep(0, num_of_portfolios)
)

set.seed(2137)
for (i in 1:num_of_portfolios) {
  
  random_weights <- runif(ncol(weights))
  random_weights <- random_weights / sum(random_weights)
  weights[i, ] <- random_weights
  
  portfolio_metrics$returns[i] <- random_weights %*% mean_returns
  portfolio_metrics$risk[i] <- sqrt(t(random_weights) %*% (cov_returns %*% random_weights))
  portfolio_metrics$sharpe[i] <- (portfolio_metrics$returns[i] - rf) / portfolio_metrics$risk[i]
  
}

optimal_portfolios <- data.frame(
  Min_Variance = c(
    round(100*weights[which.min(portfolio_metrics$risk),], 2)
    ,round(100*portfolio_metrics[which.min(portfolio_metrics$risk), ], 2) %>% as.numeric()
  )
  ,Max_Sharpe = c(
    round(100*weights[which.max(portfolio_metrics$sharpe),], 2)
    ,round(100*portfolio_metrics[which.max(portfolio_metrics$sharpe), ], 2) %>% as.numeric()
  )
) %>% t() %>% as.data.frame()

colnames(optimal_portfolios) <- c(colnames(returns), 'Expected Return', 'Standard Deviation', 'Sharpe Ratio')
optimal_portfolios

ggplot(portfolio_metrics, aes(x = risk, y = returns, color = sharpe)) +
  geom_point() +
  geom_point(aes(x = (optimal_portfolios$`Standard Deviation`[1] / 100)
                 ,y = (optimal_portfolios$`Expected Return`)[1] / 100)
             ,color = '#2ca02c', size = 5, shape = 17) +
  geom_point(aes(x = (optimal_portfolios$`Standard Deviation`[2] / 100)
                 ,y = (optimal_portfolios$`Expected Return`)[2] / 100)
             ,color = '#ff7f0e', size = 5, shape = 17) + 
  theme_bw()

min_variance_weights <- weights[which.min(portfolio_metrics$risk), ]
print(min_variance_weights)
max_sharpe_weights <- weights[which.max(portfolio_metrics$sharpe), ]
print(max_sharpe_weights)

# Perhitungan 5 hari kedepan 
saham <- matrix(c('ACES.JK', 'ADRO.JK', 'HRUM.JK', 'INTP.JK', 'ITMG.JK', 'SIDO.JK', 'TINS.JK'))
bobot <- matrix(c(0.04202698, 0.24651325, 0.07417314, 0.28746012, 0.05292759, 0.12421231, 0.17268661))
last_price <- matrix(c(750, 2850, 1885, 9950, 28975, 590, 790), nrow=7, ncol=1)
assets<-1000000000000
invest<-bobot/last_price
lembar_saham<-assets*invest
lembar_saham<-data.frame(nama_saham=saham, lembar_saham=lembar_saham)
print(lembar_saham)

saham <- matrix(c('ACES.JK', 'ADRO.JK', 'HRUM.JK', 'INTP.JK', 'ITMG.JK', 'SIDO.JK', 'TINS.JK'))
bobot <- matrix(c(00.04202698, 0.24651325, 0.07417314, 0.28746012, 0.05292759, 0.12421231, 0.17268661))
last_price <- matrix(c(785, 2810, 1820, 10100, 28675,585,780), nrow=7, ncol=1)
assets<-1000000000000
invest<-bobot/last_price
lembar_saham<-assets*invest
lembar_saham<-data.frame(nama_saham=saham, lembar_saham=lembar_saham)
print(lembar_saham)

saham <- matrix(c('ACES.JK', 'ADRO.JK', 'HRUM.JK', 'INTP.JK', 'ITMG.JK', 'SIDO.JK', 'TINS.JK'))
bobot <- matrix(c(0.04202698, 0.24651325, 0.07417314, 0.28746012, 0.05292759, 0.12421231, 0.17268661))
last_price <- matrix(c(775, 2690, 1735, 10100, 28050,585,785), nrow=7, ncol=1)
assets<-1000000000000
invest<-bobot/last_price
lembar_saham<-assets*invest
lembar_saham<-data.frame(nama_saham=saham, lembar_saham=lembar_saham)
print(lembar_saham)

saham <- matrix(c('ACES.JK', 'ADRO.JK', 'HRUM.JK', 'INTP.JK', 'ITMG.JK', 'SIDO.JK', 'TINS.JK'))
bobot <- matrix(c(0.04202698, 0.24651325, 0.07417314, 0.28746012, 0.05292759, 0.12421231, 0.17268661))
last_price <- matrix(c(750, 2660, 1695, 10025, 27200,560,770), nrow=7, ncol=1)
assets<-1000000000000
invest<-bobot/last_price
lembar_saham<-assets*invest
lembar_saham<-data.frame(nama_saham=saham, lembar_saham=lembar_saham)
print(lembar_saham)

saham <- matrix(c('ACES.JK', 'ADRO.JK', 'HRUM.JK', 'INTP.JK', 'ITMG.JK', 'SIDO.JK', 'TINS.JK'))
bobot <- matrix(c(0.04202698, 0.24651325, 0.07417314, 0.28746012, 0.05292759, 0.12421231, 0.17268661))
last_price <- matrix(c(760, 2610, 1635, 10125, 26325,605,770), nrow=7, ncol=1)
assets<-1000000000000
invest<-bobot/last_price
lembar_saham<-assets*invest
lembar_saham<-data.frame(nama_saham=saham, lembar_saham=lembar_saham)
print(lembar_saham)