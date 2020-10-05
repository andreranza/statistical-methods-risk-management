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
ticker <- as.character(unique(dataraw[,"TICKER"])) 
colnames(data)[2:(nAssets+1)] <- ticker
# From the previous chunk of code, our dataframe is called "data".
# It contains dates on the first column and prices in the others column.
# We proceed by dropping the first column.
data <- data[,-1]
prices <- data
time_t <- nrow(prices) # index for the last observation

# Log prices #
log_prices <- log(prices)

# Before we build the porfolio, we calculate different types of returns 
# and prove some identies for exercise.

# One-day net return for the whole series
one_d_net_ret <- apply(prices, 2, quantmod::Delt, type = "arithmetic")

# By adding one to the one-day net returns we obtain the one day gross return
one_d_gross_ret <- one_d_net_ret + 1

# Now we verify that we can obtain the gross return over 756 days
# either by dividing the last observed price by the first observed price, 
# or by multipying single one-day gross return along the t-1 periods

round(prices[time_t,]/prices[1,], 5) ==  round(apply(one_d_gross_ret[-1,], 2, prod), 5)

# LOG RETURNS #
# One-day log returns are obtained by applying the log function to the 
# one-day gross returns
one_d_log_ret <- apply(one_d_gross_ret, 2, log)
one_d_log_ret <- one_d_log_ret[-1,]

# Now we verify that a k period log return is simply the sum of the single period
# log returns, rather than the product as for returns.
# Calculate first the gross return over the t-1 period
# (can be done in 2 ways as noted above). We use the based on prices.
overall_gross_return <- prices[time_t,]/prices[1,] # = 1 + Rt(k)
overall_log_return <- log(overall_gross_return)
# We show now that we obtain the last quantities by simply summing over 
# the one-day log returns
overall_gross_return_b <- apply(one_d_log_ret, 2, sum)
# We try the identity:
round(overall_log_return, 5) == round(overall_gross_return_b, 5)

# Now we proceed with the portfolio construction under costant holdings.


library(GGally)
library(QRM)
library(copula)
######## COPULAS #########

# The dataset used in this section is 'one_d_log_ret'.
# It contains log returns for each day on each stock.
# The NA has already been removed.
cond <- apply(one_d_log_ret, 1, function(x) any(x == 0))
zero_index <- which(cond == TRUE)
# Keep only the rows in which returns for each stock are different
# from zero.
one_d_log_ret_cop <- one_d_log_ret[-zero_index,]
dim(one_d_log_ret_cop)

# select financial stocks
financials_log_ret <- one_d_log_ret_cop[,c("BA","WFC","NTRS","AIG","MS","GS")]


# Rank Correlations
ggcorr(financials_log_ret, method = c("pairwise", "pearson"), label = TRUE) +
  ggtitle("Pearson")

ggcorr(financials_log_ret, method = c("pairwise", "kendal"), label = TRUE) +
  ggtitle("Kendall's tau")

ggcorr(financials_log_ret, method = c("pairwise", "spearman"), label = TRUE) +
  ggtitle("Spearman")


#### EMPIRICAL COPULA ####
## Semi-parametric estimation ##

# A copula is a multivariate cumulative distribution function
# with standard uniform margins.
# We apply the probability transfrom to each margin.
# We estimate the cdf of each margin from the data in 
# order to perform the transformation by applying
# the estimated cdf to our data.
# In this way we get aprox. uniform margins.
# Having applied the transformation, we fit different types 
# of copulas, such as Gaussian, t Copula, Clayton and Gumbel,
# we want to understand which one fits the data better.

#### Pseudo-observations ####
pseudo_obs <- copula::pobs(financials_log_ret)
# OR: pseudo_obs_a <- apply(financials_log_ret, 2, QRM::edf, adjust = 1)
ppi <- 320
setwd("/Users/Andrea/__LSE_RISK/images")
png("1_pseudo_observations.png",width = 10*ppi, height = 10*ppi, res = ppi)
splom2(pseudo_obs, cex = 0.2, col.mat = 'black', pch = 16)
dev.off()
# The marginal are close to uniform. 

# Assess presence of tail dependence by transforming the margins to 
# normal scores
pseudo_obs_tail <- apply(pseudo_obs, 2, qnorm)
png("2_copula_normal_transformation.png", width = 10*ppi, height = 10*ppi, res = ppi)
splom2(pseudo_obs_tail, cex = 0.2, col.mat = 'black', pch = 16)
dev.off()
# NO relevant tail dependence is assessed

#### Normal copula using "QRM" ####
gauss_copula_qrm <- QRM::fit.gausscopula(pseudo_obs)
gauss_copula_qrm

#### Normal Copula using "copula" ####
summary(fitCopula(normalCopula(dim = 6, dispstr = "un"), data = pseudo_obs, method = "mpl")) # 1209
#  We are looking for the linear correlation matrix that max the log-lik func

#### Student-t Copula ####
# We search over the correlation matrices and degrees of freedom
t_copula <- fitCopula(tCopula(dim = 6, dispstr = "un"), data = pseudo_obs, method = "mpl")
summary(t_copula)
# 1589

#### Gumbel Copula ####
summary(fitCopula(gumbelCopula(dim = 6), data = pseudo_obs, method = "mpl"))
# 902.1

#### Clayton Copula ####
summary(fitCopula(claytonCopula(dim = 6), data = pseudo_obs, method = "mpl"))
# 885.1 




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
library(GGally)
library(QRM)
######### COPULA #########
rm(list=ls(all=TRUE)) 
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
head(data)
class(data)

#### ASSETS RETURNS ####
asset.returns <- apply(data[,-1], 2, quantmod::Delt, type = 'log') # Delt keeps NA
cond <- apply(asset.returns, 1, function(x) any(x == 0))
zero_index <- which(cond == TRUE)
asset.returns <- asset.returns[-zero_index,]
dim(asset.returns)

# copula <- as.data.frame(apply(asset.returns, 2, edf, adjust=1))

# ggpairs(copula, lower = list(continuous = wrap("points", size = 0.10))) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank())

# ggcorr(copula, method = c("pairwise", "kendal"), label = TRUE) +
#   ggtitle("Kendal")
# ggcorr(copula, method = c("pairwise", "pearson"), label = TRUE) +
#   ggtitle("Pearson")
# ggcorr(copula, method = c("pairwise", "spearman"), label = TRUE) +
#   ggtitle("Spearman")

#### ms vs GS - X3 ###
X <- asset.returns[-1,c("MS","GS","NTRS", "WFC","AMZN", "MSFT")]
X <- apply(X, 2, edf, adjust = 1)

ggpairs(as.data.frame(X), lower = list(continuous = wrap("points", size = 0.10))) +
   theme_bw() +
   theme(panel.grid.major = element_blank())

copulaXGauss <- fit.gausscopula(X) # fit copula with mle with optimization on loglik <- it finds a matrix
copulaXGauss # if you change P the likelihood will be different
# t copula
copulaXt <- fit.tcopula(X, method = 'Kendall') # you also have to find optim P and optim v
# we have estimated from the data
copulaXt # you don't have variance defined because not greater than 2 so lot of outlier probabliy

# 2-dimensional Archimedian copulas (Gumbel and Clayton)
copulaXGumb <- fit.AC(X,"gumbel") # thaeta is estimated from the data # tail dep depends on theta
copulaXClay <- fit.AC(X,"clayton")

Y <- asset.returns[,c("MS","GS")]
Y <- apply(Y,2,edf,adjust=1)
copulaYGumb <- fit.AC(Y, "gumbel")
copulaYt <- fit.tcopula(copulaY, method="Kendall")
