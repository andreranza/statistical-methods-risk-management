library(ggplot2)
rm(list=ls(all=TRUE))
dataraw <- read.csv("/Users/Andrea/__LSE_RISK/Data_project_adj_2.csv")
ids <- unique(dataraw[,1])
nAssets <- length(ids)
nObs <- nrow(dataraw)/nAssets
data <- matrix(NA, nrow = nObs, ncol = nAssets + 1)
data[,1] <- dataraw[dataraw[,1] == ids[1], "date"]
for (i in 1:nAssets){
  data[,i+1]<-dataraw[dataraw[,1] == ids[i], "PRC"]
}
head(data)
Date <- as.numeric(data[,1])
ticker <- as.character(unique(dataraw[,"TICKER"])); 
colnames(data)[2:(nAssets+1)] <- ticker
# From the previous chunk of code, our dataframe is called "data".
# It contains dates on the first column and prices in the others column.
# We proceed by dropping the first column.
data <- data[,-1]
prices <- data
time_t <- nrow(prices) # index for the last observation

# Log prices #
log_prices <- log(prices)
head(log_prices)
# Before we build the porfolio, we calculate different types of returns 
# and prove some identies for exercise.

# One-day net return for the whole series
one_d_net_ret <- apply(prices, 2, quantmod::Delt, type = "arithmetic")
head(one_d_net_ret)
# By adding one to the one-day net returns we obtain the one day gross return
one_d_gross_ret <- one_d_net_ret + 1
head(one_d_gross_ret)
# Now we verify that we can obtain the gross return over 756 days
# either by dividing the last observed price by the first observed price, 
# or by multipying single one-day gross return along the t-1 periods
# 
round(prices[time_t,]/prices[1,], 5) ==  round(apply(one_d_gross_ret[-1,], 2, prod), 5)

# LOG RETURNS #
# One-day log returns are obtained by applying the log function to the 
# one-day gross returns
one_d_log_ret <- apply(one_d_gross_ret, 2, log)
head(one_d_log_ret)
one_d_log_ret <- one_d_log_ret[-1,]

# Now we verify that a k period log return is simply the sum of the single period
# log returns, rather than the product as for returns.
# Calculate first the gross return over the t-1 period
# (can be done in 2 ways as noted above). We use the based on prices.
overall_gross_return <- prices[time_t,]/prices[1,] # = 1 + Rt(k)
overall_log_return <- log(overall_gross_return)
# We show now that we obtain the last quantity by simply summing over 
# the one-day log returns
overall_gross_return_b <- apply(one_d_log_ret, 2, sum)
# We try the identity:
round(overall_log_return, 5) == round(overall_gross_return_b, 5)

# Now we proceed with the portfolio construction under costant holdings.
###### PORTFOLIO  ######
# The value of the portfolio at the beginning of the period will be 10.000$
# I.e. the first observation in our time-series
single_k <- rep(1000, dim(prices)[2]) # 1000$ on each asset
pf_value_1 <- sum(single_k)

# We calculate how many stocks we get at the first period.
# THE NUMBER OF UNITS for each ASSET IS DETERMINED AT T=1, THEN IT IS FIXED #
# ALONG THE SUCCESIVE PERIODS #
n_asset <- as.matrix(single_k/prices[1,])
n_asset
# PORTFOLIO VALUE at each time period
PV <- prices%*%n_asset
head(PV)

# We calculate the matrix of weights
n_asset_b <- matrix(rep(t(n_asset), dim(prices)[1]), 
                    ncol = dim(prices)[2], 
                    nrow = dim(prices)[1], byrow = TRUE)
PV_b <- matrix(rep(PV, dim(prices)[2]), 
               ncol = dim(prices)[2], 
               nrow = dim(prices)[1], 
               byrow = FALSE)
weights <- (prices*n_asset_b)/PV_b

# We verify that the weights sum to one at each day
sum(apply(weights, 1, sum)) == sum(rep(1, dim(prices)[1]))

# NET PORTFOLIO RETURNS at each time period #
net_pv_ret <- apply(PV, 2, quantmod::Delt, type = 'arithmetic')

# GROSS PORTFOLIO RETURNS at each time period #
gross_pv_ret <- net_pv_ret + 1

# LOG PORTFOLIO RETURNS at each time period #
log_pv_ret <- apply(gross_pv_ret, 2, log)

# absolute LOSESS #
losses <- -diff(PV)

# Compare "net_pv_ret" vs "log_pv_ret"
head(cbind(net_pv_ret,log_pv_ret))

##### DAILY NORMAL STATIC VAR 0.95 (Method which is not based on linearization) #####
# normal linear var model assumes that dist of returns is linear #
# normal linear model only when portfolio returns is linear function of its risk factors returns #
# basic assumption returns on the portfolio are iid with normal distribution
# co-dependencies between structure are assumed to be represented by correlations #
date <- as.Date(as.character(Date[1:dim(weights)[1]]), "%Y%m%d")

head(date)

# As a convention, we take losses to be positive
ts_var <- xts::xts(-net_pv_ret, date)
ts_var <- ts_var[-1,]

mean_loss <- mean(ts_var)
sd_loss <- sqrt(var(ts_var))

# One-day NORMAL VAR at 95% level expressed as a percentage of portfolio value #
var_gauss <- qnorm(0.95)*sd_loss - mean_loss

# One-day HISTORICAL VAR at 95% level expressed as a percentage of portfolio value #
var_hist <- quantile(ts_var, p = 0.95)

# Last observed portfolio value
last_pv <- PV[dim(PV)[1],]

# One-day ACTUAL NORMAL VAR 
actual_normal_var <- as.numeric(var_gauss * last_pv)

# One-day ACTUAL HISTORICAL VAR
actual_hist_var <- as.numeric(var_hist * last_pv)

#cbind(actual_normal_var, actual_hist_var)




#### Graphical comparison of Normal vs Historical VaR ####
library(ggplot2)

net_pv_ret_df <- as.data.frame(-net_pv_ret)
colnames(net_pv_ret_df) <- "Losses"
mean_loss <- mean(net_pv_ret_df$Losses, na.rm = TRUE)
sd_loss <- sd(net_pv_ret_df$Losses, na.rm = TRUE)

dx <- density(net_pv_ret_df$Losses, na.rm = TRUE)
xnew <- var_hist
y_hist <- approx(dx$x, dx$y, xout = xnew)

df <- data.frame(x1 = var_hist, y1 = y_hist$y, 
                 x2 = var_gauss, y2 = dnorm(var_gauss, mean = mean_loss, sd = sd_loss))

ggplot(net_pv_ret_df, aes(x = Losses)) + 
  geom_density(colour = '#525252') +
  theme_bw() +
  xlab('Portfolio value percenual Loss (%)') +
  stat_function(fun = dnorm, 
                args = list(mean = mean_loss, sd = sd_loss),
                colour = "red") +
  geom_segment(aes(x = x1, xend = x1, y = 0, yend = y1), colour = "#525252", data = df) +
  geom_segment(aes(x = x2, xend = x2, y = 0, yend = y2), colour = 'red', data = df) +
  #annotate('text', x = 0.05, y = 5, label = 'VaR 95%\nNormal', colour = 'red') +
  #annotate('text', x = 0.025, y = 5, label = 'VaR 95%\nHistorical', colour = 'black') +
  ggtitle('one-day Historical 95% VaR\nVS\none-day Normal VaR') +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggsave("historical_vs_normal.png", device = "png", dpi = 320, width=297, height = 210, units = "mm")




### fine da MARKDOWN####
###########################################################################################################
##############################################################################################################
#############################################################################################################
###########################################################################################################
##############################################################################################################
#############################################################################################################
###########################################################################################################
##############################################################################################################
#############################################################################################################
###########################################################################################################
##############################################################################################################
#############################################################################################################
library(tidyverse)
library(quantmod)
rm(list=ls(all=TRUE)) 

#### Read Data ####
dataraw <- read.csv("/Users/Andrea/__LSE_RISK/Data_project_adj_2.csv")
ids <- unique(dataraw[,1])
nAssets <- length(ids)
nObs <- nrow(dataraw)/nAssets
data <- matrix(NA, nrow = nObs, ncol = nAssets + 1)
data[,1] <- dataraw[dataraw[,1] == ids[1], "date"]
for (i in 1:nAssets){
  data[,i+1] <- dataraw[dataraw[,1] == ids[i], "PRC"]
}
ticker <- as.character(unique(dataraw[,"TICKER"]))
colnames(data)[2:(nAssets+1)] <- ticker
colnames(data)[1] <- 'Date'
prices <- data
Time_t <- dim(prices)[1]

###### PORTFOLIO  ######
# The first day $1000 are invested in each stock - weights are kept constant over time #
inv <- rep(1000, dim(prices)[2])
pf.weights <- inv/sum(inv)
asset.returns <- apply(prices, 2, quantmod::Delt) # Asset Returns
pf.returns <- as.matrix(apply(asset.returns*pf.weights, 1, sum), ncol = 1) # Pf. returns
loss <- -(pf.returns*sum(inv)) # Pf. loss
colnames(loss)[1] <- c('P_and_L'); loss <- as.data.frame(loss)

##### DAILY NORMAL VAR 0.95  #####
p <- 0.95
meanloss <- apply(loss, 2, mean, na.rm = TRUE); varloss <- apply(loss, 2, var, na.rm = TRUE)
VaR.normal <- meanloss + sqrt(varloss) * qnorm(p)

##### HISTORICAL VAR #####
VaR.hs <- quantile(loss,p, na.rm = TRUE)

ggplot(loss, aes(y = P_and_L)) + 
  geom_boxplot() +
  theme_bw() +
  ggtitle(" Portfolio 'P&L' Box Plot\nJan 03, 2007 - Dec 31, 2009") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab('Loss') + 
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank()) +
  coord_flip() 

#### Outlier detection ####
outlier <- loss$P_and_L < -5000
loss <- loss[!outlier,]
loss <- as.data.frame(loss)
colnames(loss)[1] <- 'P_and_L'

# Histogram 
ggplot(loss, aes(x = P_and_L)) + 
  geom_histogram(fill = '#eeeeee', colour = 'black') +
  theme_bw() +
  xlab('Loss') +
  ggtitle('Loss Frequency Distrbution\nJan 03, 2007 - Dec 31, 2009') +
  theme(plot.title = element_text(hjust = 0.5))

vertical.lines <- c(meanloss, VaR.normal, VaR.hs)
ggplot(loss, aes(x = P_and_L)) + 
  geom_density(colour = '#525252') +
  theme_bw() +
  xlab('Loss') +
  stat_function(fun = dnorm, 
                args = list(mean = meanloss, sd = sqrt(varloss)),
                colour = "red") +
  coord_cartesian(xlim = c(-2000, 2000)) +
  geom_vline(xintercept = vertical.lines,
             linetype = 4, size = 0.5,
             colour = c('black','red', 'black')) +
  annotate('text', x = meanloss, 
           y = 0.001/2, 
           label = 'Avg. Loss', 
           size = 3) +
  annotate('text', x = as.numeric(VaR.normal), 
           y = 0.002, 
           label = 'VaR 95%\nNormal',
           colour = 'red') +
  annotate('text', x = VaR.hs, 
           y = 0.002, 
           label = 'VaR 95%\nHistorical',
           colour = 'black') +
  ggtitle('Density of Historical Portfolio P&L Over Two Years\nAnd 5% 1-Day Historical and Normal VaR') +
  theme(plot.title = element_text(hjust = 0.5))

median(VaR.hs)


