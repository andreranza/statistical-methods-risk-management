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
ticker <- as.character(unique(dataraw[,"TICKER"])); colnames(data)[2:(nAssets+1)] <- ticker
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

# time plot of portfolio value
setwd("/Users/Andrea/__LSE_RISK/images")
ppi <- 320
png("portfolio_value.png", width=10*ppi,height=10*ppi*(70/99),res=ppi)
plot(PV, type = 'l')
dev.off()
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
ppi <- 320
png("net_portfolio_returns.png", width=10*ppi,height=10*ppi*(70/99),res=ppi)
plot(net_pv_ret, type = 'l')
dev.off()
# GROSS PORTFOLIO RETURNS at each time period #
gross_pv_ret <- net_pv_ret + 1
ppi <- 320
png("gross_portfolio_returns.png", width=10*ppi,height=10*ppi*(70/99),res=ppi)
plot(gross_pv_ret, type = 'l')
dev.off()
# LOG PORTFOLIO RETURNS at each time period #
png("log_portfolio_returns.png", width=10*ppi,height=10*ppi*(70/99),res=ppi)
log_pv_ret <- apply(gross_pv_ret, 2, log)
plot(log_pv_ret, type = 'l')
dev.off()
# absolute LOSESS #
losses <- -diff(PV)

# Compare "net_pv_ret" vs "log_pv_ret"
head(cbind(net_pv_ret,log_pv_ret))

losses <- as.data.frame(losses)
ggplot(-losses, aes(y = V1, x = 1)) + 
  geom_boxplot() + 
  theme_bw() +
  ggtitle(" Portfolio 'P&L' Distribution\nJan 03, 2007 - Dec 31, 2009") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  coord_flip() +
  ylab('Loss') + 
  scale_x_continuous(breaks=NULL) + 
  theme(axis.title.x = element_blank()) +
  ggsave("boxplot_profit_and_losses.png", device = "png", dpi = 320, width=297, height = 210, units = "mm")
