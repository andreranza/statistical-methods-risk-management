#### LOAD TEST TIME SERIES - data_test ####
dataraw_test <- read.csv("/Users/Andrea/__LSE_RISK/VaR_Backtesting.csv")
ids <- unique(dataraw_test[,1])
nAssets <- length(ids)
nObs <- nrow(dataraw_test)/nAssets; data_test <- matrix(NA, nrow = nObs, ncol = nAssets + 1)
data_test[,1] <- dataraw_test[dataraw_test[,1] == ids[1], "date"]
for (i in 1:nAssets){
  data_test[,i+1]<-dataraw_test[dataraw_test[,1] == ids[i], "PRC"]
}
ticker <- as.character(unique(dataraw_test[,"TICKER"]))
colnames(data_test)[2:(nAssets+1)] <- ticker
colnames(data_test)[1] <- 'Date' 
dates_test <- data_test[,1]
dates_test <- as.Date(as.character(dates_test), format = "%Y%m%d")
data_test <- data_test[,c(-1,-10)] # remove an empty column and dates

###### Merging 'data' and 'data_test' - entire #####
entire <- rbind(data, data_test)

###### PORTFOLIO including TEST series ######
# Portfolio value at each time t #
PV_test <- entire%*%n_asset

# Instrumental matrix to compute the weights #
n_asset_c <- matrix(rep(t(n_asset), dim(entire)[1]), 
                    ncol = dim(entire)[2], 
                    nrow = dim(entire)[1], byrow = TRUE)

PV_c <- matrix(rep(PV_test, dim(entire)[2]), 
               ncol = dim(entire)[2], 
               nrow = dim(entire)[1], 
               byrow = FALSE)

# Matrix containg weights #
weights_test <- (entire*n_asset_c)/PV_c

sum(apply(weights_test, 1, sum)) == dim(weights_test)[1]

# Net Portfolio returns and losses #
entire_ret <- apply(PV_test, 2, quantmod::Delt, type = 'arithmetic')
entire_losses <- -entire_ret

#### TIME SERIES of THE LOSSES ####
dates_entire <- as.Date(c(date, dates_test))
dates_entire <- lubridate::ymd(dates_entire)
entire_losses_ts <- xts::xts(entire_losses, dates_entire)

##### BACKTEST - rolling window #####
VaR_t_Normal <- function(vec, alpha) {
  mu.hat = mean(vec, na.rm = TRUE)
  sd = sd(vec, na.rm = TRUE)
  VaR = mu.hat + sd * qnorm(alpha)
  return(VaR)
}

VaR_t_hs <- function(vec, alpha) {
  VaR = quantile(vec, alpha, na.rm = TRUE)
  return(VaR)
}

# I remove the first from the first row of entire_loss_ts which is NA #
entire_losses_ts <- entire_losses_ts[-1,]

# I set the rolling window to be of size 755
Time_t <- dim(data)[1]-1

#### NORMAL VAR ####
# By setting align 'right', the function rollapply start counting
# from the first row in order to define the sample on which it will 
# estimate the parameters needed to compute the VaR
Daily_norm_Var <- zoo::rollapply(entire_losses_ts, width = Time_t, align = 'right', FUN = VaR_t_Normal, alpha = 0.95)

# We shift forward by one day the series because we are calculating the VaR
# over one-day risk horizon. However, rollapply locates the first result 
# in correspondence of the time instant from which we account for the risk 
# horizon. This operation allow to make a comparison between the realized
# loss and the one-day VaR.
Daily_norm_Var <- lag(Daily_norm_Var, k = 1)

#### HISTORICAL VAR ####
Daily_hs_Var <- zoo::rollapply(entire_losses_ts, width = Time_t, align = 'right', FUN = VaR_t_hs, alpha = 0.95)
Daily_hs_Var <- lag(Daily_hs_Var, k = 1)

# We select the range to be the entire 2010 because it is our test
loss.2010 <- window(entire_losses_ts, start = as.Date('2010-01-04'), end = as.Date('2010-12-31'))
norm.Var_2010 <- window(Daily_norm_Var, start = as.Date('2010-01-04'), end = as.Date('2010-12-31'))
hs.Var_2010 <- window(Daily_hs_Var, start = as.Date('2010-01-04'), end = as.Date('2010-12-31'))

setwd("/Users/Andrea/__LSE_RISK/images")
ppi <- 320
png("var_backtest.png", width=10*ppi,height=10*ppi*(70/99),res=ppi)
par(mfrow=c(2,1))
plot(merge(loss.2010, norm.Var_2010), main = "95% 1d VaR and P&L - Normal Method")
plot(merge(loss.2010, hs.Var_2010), main = "95% 1d VaR and P&L - Historical Method")
dev.off()
# We count and compare the violations
n_violations_hs <- sum(entire_losses_ts > hs.Var_2010, na.rm = TRUE)
n_violations_norm <- sum(entire_losses_ts > norm.Var_2010, na.rm = TRUE)

df_violations <- data.frame(hs = n_violations_hs, norm = n_violations_norm)
colnames(df_violations)[1] <- "Numb. of violations - Historical"
colnames(df_violations)[2] <- "Numb. of violations - Normal"













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
library(lubridate)

######### VAR BACKTESTING #########

rm(list=ls(all=TRUE)) 


#### LOAD FIRST TIME SERIES - data ####
dataraw <- read.csv("Data_project_adj_2.csv")
names(dataraw) 
ids <- unique(dataraw[,1])
nAssets <- length(ids)
nObs <- nrow(dataraw)/nAssets
data <- matrix(NA, nrow = nObs, ncol = nAssets + 1)
data[,1] <- dataraw[dataraw[,1] == ids[1], "date"]
for (i in 1:nAssets){
  data[,i+1]<-dataraw[dataraw[,1] == ids[i], "PRC"]
}
ticker <- as.character(unique(dataraw[,"TICKER"]))
colnames(data)[2:(nAssets+1)] <- ticker
colnames(data)[1] <- 'Date'
Date <- data[,1]
#### LOAD TEST TIME SERIES - data_test ####
dataraw_test <- read.csv("VaR_Backtesting.csv")
head(dataraw_test)
sum(is.na(dataraw_test))
names(dataraw_test) 
ids <- unique(dataraw_test[,1])
nAssets <- length(ids)
nObs <- nrow(dataraw_test)/nAssets
data_test <- matrix(NA, nrow = nObs, ncol = nAssets + 1)
data_test[,1] <- dataraw_test[dataraw_test[,1] == ids[1], "date"]
for (i in 1:nAssets){
  data_test[,i+1]<-dataraw_test[dataraw_test[,1] == ids[i], "PRC"]
}
ticker <- as.character(unique(dataraw_test[,"TICKER"]))
colnames(data_test)[2:(nAssets+1)] <- ticker
colnames(data_test)[1] <- 'Date'
Date <- data_test[,1]
data_test <- data_test[,-10] # remove an empty column
head(data_test)
sum(is.na(data_test))
class(data_test)
###### Merge of the two ts - entire #####
entire <- rbind(data, data_test)
head(entire)
dim(entire)
###### PORTFOLIO  ######
# at the starting date $1000 are invested in each stock
#### ie the portfolio is not rebalanced ####
inv <- rep(1000, dim(entire[,-1])[2])
# weights, kept constant over time
pf.weights <- inv/sum(inv)

#### ASSETS RETURNS ####
asset.returns <- apply(entire[,-1], 2, quantmod::Delt) # Delt keeps NA
#### PORTFOLIO RETURNS - constant weights ####
pf.returns <- as.matrix(apply(asset.returns*pf.weights, 1, sum), ncol = 1)
##### PORFOLIO LOSSES ####
loss <- -(pf.returns*sum(inv))

#### TIME SERIES of THE LOSSES p&L
dates <- entire[,1]
dates <- lubridate::ymd(dates)
loss.ts <- xts(loss, dates)
head(loss.ts)

##### BACKTEST - rolling window #####
# a time series has been created in order to be able to use the rollapply function

VaR_t_Normal <- function(vec, alpha) {
  mu.hat = mean(vec, na.rm = TRUE)
  sd = sd(vec, na.rm = TRUE)
  VaR = mu.hat + sd * qnorm(alpha)
  return(VaR)
}

VaR_t_hs <- function(vec, alpha) {
  VaR = quantile(vec, alpha, na.rm = TRUE)
  return(VaR)
}

VaR_t_Normal(loss, 0.95)
VaR_t_hs(loss, 0.95)
Time_t <- dim(data)[1]
# daily Normal var at 0.95 is calculated using a rolling window of length 756 
# to estimate the VaR which corresponds to the sample 
#
# the following is daily var calculated starting at time_t for the subsequent day
# to compare the series should be moved by one step

Daily_norm_Var <- rollapply(loss.ts, width = Time_t, 
                              align = 'right', 
                              FUN = VaR_t_Normal, alpha = 0.95)
Daily_norm_Var <- lag(Daily_norm_Var, k = 1)

Daily_hs_Var <- rollapply(loss.ts, width = Time_t,
                          align = 'right',
                          FUN = VaR_t_hs, alpha = 0.95)
Daily_hs_Var <- lag(Daily_hs_Var, k = 1)
loss.2010 <- window(loss.ts, start = as.Date('2010-01-04'), end = as.Date('2010-12-31'))
norm.Var_2010 <- window(Daily_norm_Var, start = as.Date('2010-01-04'), end = as.Date('2010-12-31'))
hs.Var_2010 <- window(Daily_hs_Var, start = as.Date('2010-01-04'), end = as.Date('2010-12-31'))
par(mfrow = c(2,1))
plot(merge(loss.2010, norm.Var_2010), main = "95% daily VaR and P&L - Historical Method")
plot(merge(loss.2010, hs.Var_2010), main = "95% daily VaR and P&L - Normal Method", ylim = c(-1000,1000))
mean(loss.ts > hs.Var_2010, na.rm = TRUE) / length(hs.Var_2010)

# overestimating the risk





