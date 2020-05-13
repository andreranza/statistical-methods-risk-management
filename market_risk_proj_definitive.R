rm(list=ls()) 
library(xts)
library(stringr)
library(tidyverse)
library(highcharter)
library(GGally)

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
dates <- as.Date(as.character(data[,1]), "%Y%m%d")
data <- data[,-1]

colnames(data) <- ticker
ts <- xts(data, dates)
#### prices ####
highchart(type = "stock") %>% 
  hc_title(text = "Prices") %>% 
  hc_add_series(ts[, ticker[1]], name = ticker[1]) %>% 
  hc_add_series(ts[, ticker[2]], name = ticker[2]) %>%   
  hc_add_series(ts[, ticker[3]], name = ticker[3]) %>%
  hc_add_series(ts[, ticker[4]], name = ticker[4]) %>%
  hc_add_series(ts[, ticker[5]], name = ticker[5]) %>%
  hc_add_series(ts[, ticker[6]], name = ticker[6]) %>%
  hc_add_series(ts[, ticker[8]], name = ticker[8]) %>%
  hc_add_series(ts[, ticker[9]], name = ticker[9]) %>%
  hc_add_series(ts[, ticker[10]], name = ticker[10]) %>%
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = TRUE) 

ts["2008-09-15"]

sub_ts <- window(ts, startdate="2008-03-14",end="2009-01-20")
head(sub_ts)
tail(sub_ts)
first <- apply(sub_ts,2,first)
last <- apply(sub_ts,2,last)
((last-first)/first)*100

#### log returns and percentage returns ####
log_rts <- apply(ts, 2, quantmod::Delt, type = 'log')
net_rts <- apply(ts,2,quantmod::Delt,type = 'arithmetic')
head(log_rts)
head(net_rts)

#### Test for multivariate normality (Mardia) ####
normality_test <- MVN::mvn(data = log_rts, mvnTest = "mardia")
normality_test$multivariateNormality
normality_test <- MVN::mvn(data = log_rts[-1,], mvnTest = "mardia", multivariatePlot = "qq")


#### Test for marginal normality (Jarque - Bera) ####
JBTest <- function(dataMatrix){
  ncol <- dim(dataMatrix)[2]
  pvals <- array(NA, ncol)
  for (i in 1:ncol) pvals[i] <- tseries::jarque.bera.test(dataMatrix[,i])$p.value
  if (!is.null(names(dataMatrix))) names(pvals) <- names(dataMatrix)
  return(pvals)
}
JBTest(log_rts[-1,])
apply(log_rts[-1,], 2, tseries::jarque.bera.test)

#### Test for marginal student t with 1 dgf ####
uniques <- apply(log_rts, 2, unique)
values_t_test <- vector(mode="list",length = dim(log_rts)[2])
for(i in 1:dim(log_rts)[2]){
  values_t_test[[i]] <- ks.test(uniques[[i]], "pt", df = 1)
}
results_t_test <- list()
results_t_test[colnames(log_rts)] <- values_t_test
results_t_test

par(mfrow=c(3,4))
for(i in 1:dim(log_rts)[2]){
  plot(qt(ppoints(log_rts[-1,i]),10), sort(log_rts[-1,i]), 
       main=str_c("Student-t QQ (dof=10) Plot for",
                  colnames(log_rts)[i],sep=" "),
       xlab="Theoretical quantiles",ylab="Sample Quantiles")
  abline(a=0,b=1)
}

#### FINANCIAL STOCKS ####
fin.stocks <- as.data.frame(log_rts[,c("BA","WFC","NTRS","AIG","MS","GS")])
fin.scatter <- ggpairs(fin.stocks, 
                       lower = list(continuous = wrap("points", size = 0.1))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        strip.text = element_text(face="bold", size=rel(0.60)),
        plot.title=element_text(size = rel(1.1), 
                                lineheight = .9, 
                                family = "Helvetica",
                                face = "plain", 
                                colour = "black",
                                hjust = 0.5)) +
  ggtitle("Pair-wise Stock Returns Financial stocks")
head(fin.stocks)


#### Mardia test for combinations of bivariate distributions of stocks ####
combs <- t(combn(1:dim(fin.stocks)[2],2))
values <- vector(mode="list", length = dim(combs)[1])
names <- c()
for (i in 1:dim(combs)[1]) {
  names[i] <- str_c(colnames(fin.stocks[,combs[i,]]), collapse="_")
  values[[i]] <- MVN::mvn(fin.stocks[-1,combs[i,]], 
                          mvnTest = "mardia")$multivariateNormality
}
results <- list()
results[names] <- values
results


#########################
######## COPULAS ########
#########################
# Semi-parametric estimation -> specify: method="mpl"
############# MULTIVARIATE COPULA OF DIMENSION 6 ##########
# Keep only the rows in which returns for each stock are different
# from zero.
cond <- apply(fin.stocks, 1, function(x) any(x == 0))
zero_index <- which(cond == TRUE)
fin.stocks <- fin.stocks[-zero_index,]
dim(log_rts)
library(QRM)
library(copula)
#### PSEUDO-OBSERVATIONS ####
#pseudo_obs <- apply(fin.stocks, 2, QRM::edf, adjust = 1)
pseudo_obs <- copula::pobs(fin.stocks)
splom2(pseudo_obs, cex = 0.2, col.mat = 'black', pch = 16)

par(mfrow = c(3,3))
for(i in 1:6){
  hist(pseudo_obs[,1])
}

#### NORMAL ####
normal_copula <- fitCopula(normalCopula(dim = 6, dispstr = "un"), 
                           data = pseudo_obs[-1,], 
                           method = "mpl") # 1254
summary(normal_copula)
#  We are looking for the linear correlation matrix that max the log-lik func

#### STUDENT-T ####
# We search over the correlation matrices and degrees of freedom
t_copula <- fitCopula(tCopula(dim = 6, dispstr = "un"), data = pseudo_obs[-1,], 
                      method = "mpl")
summary(t_copula)
# 1596

#### GUMBEL  ####
summary(fitCopula(gumbelCopula(dim = 6), data = pseudo_obs[-1,],
                  method = "mpl"))
# 911.2

#### CLAYTON ####
summary(fitCopula(claytonCopula(dim = 6), data = pseudo_obs[-1,], 
                  method = "mpl"))
#884.6 


########### BIVARIATE COMBINATIONS #################
combs <- t(combn(1:dim(fin.stocks)[2],2))

#### BIVARIATE STUDENT T COPULA ####
values_biv_t_cop <- vector(mode="list", length = dim(combs)[1])
for (i in 1:dim(combs)[1]) {
  values_biv_t_cop[[i]] <- fitCopula(tCopula(dim = 2, dispstr = "un"), 
                                     data = pseudo_obs[-1,combs[i,]], 
                                     method = "mpl")
}
results_biv_t_cop <- list()
results_biv_t_cop[names] <- values_biv_t_cop
results_biv_t_cop
# Simulation of t Copula #
nObs <- 709
tNVar <- 6
cor.par <- 0.7
# dispersion matrix
tSigma <- (1-cor.par)*diag(tNVar)+array(cor.par,c(tNVar,tNVar))
t.dof <- 4
# multivariate dim 6 student t distribution
tData <- rmt(nObs,t.dof,mu=array(0,tNVar),tSigma)
head(tData)
tCopulaData <- apply(tData,2,pt,df=t.dof)
splom2(tCopulaData, cex = 0.2, col.mat = 'black', pch = 16)


# For displaying contour charts
distribution.limits <- c(0.01,0.99)
Sigma.contour <- equicorr(2,0.5)
threed.limits <- c(-3,3)
par(mfrow = c(3,3))

for (i in 1:6){
  hist(tCopulaData[,i])
}

par(mfrow = c(1,2))
BiDensPlot(func=dcopula.t,xpts=distribution.limits,ypts=distribution.limits,
           df=t.dof,Sigma=Sigma.contour, type = 'contour')
BiDensPlot(func=dcopula.t,xpts=distribution.limits,ypts=distribution.limits,
           df=t.dof,Sigma=Sigma.contour, type = 'persp',npts=15)


#### BIVARIATE NORMAL COPULA ####
values_biv_normal_cop <- vector(mode="list", length = dim(combs)[1])
for (i in 1:dim(combs)[1]) {
  values_biv_normal_cop[[i]] <- fitCopula(normalCopula(dim = 2), 
                                          data = pseudo_obs[-1,combs[i,]],
                                          method = "mpl")
}
results_biv_normal_cop <- list()
results_biv_normal_cop[names] <- values_biv_normal_cop
results_biv_normal_cop

#### BIVARIATE GUMBEL COPULA ####
values_biv_gumbel_cop <- vector(mode="list", length = dim(combs)[1])
for (i in 1:dim(combs)[1]) {
  values_biv_gumbel_cop[[i]] <- fitCopula(gumbelCopula(dim = 2), 
                                          data = pseudo_obs[-1,combs[i,]],
                                          method = "mpl")
}
results_biv_gumb_cop <- list()
results_biv_gumb_cop[names] <- values_biv_gumbel_cop
results_biv_gumb_cop

#### BIVARIATE CLAYTON COPULA ####
values_biv_clayton_cop <- vector(mode="list", length = dim(combs)[1])
for (i in 1:dim(combs)[1]) {
  values_biv_normal_cop[[i]] <- fitCopula(claytonCopula(dim = 2), 
                                          data = pseudo_obs[-1,combs[i,]], method = "mpl")
}
results_biv_clayton_cop <- list()
results_biv_clayton_cop[names] <- values_biv_clayton_cop
results_biv_clayton_cop


###### PORTFOLIO constant holdings ######
# The value of the portfolio at the beginning of the period will be 10.000$
# I.e. the first observation in our time-series
prices <- data
head(prices)
single_k <- rep(1000, dim(prices)[2])
pf_value_1 <- sum(single_k)

# We calculate how many stocks we get at the first period (units or portholdings).
# THE NUMBER OF ASSET IS DETERMINED AT T=1, THEN IT IS FIXED #
# ALONG THE SUCCESIVE PERIODS #

n_asset <- as.matrix(single_k/prices[1,])

# PORTFOLIO VALUE at each time period
PV <- prices%*%n_asset

# We calculate the matrix of weights
n_asset_b <- matrix(rep(t(n_asset), dim(prices)[1]), 
                    ncol = dim(prices)[2], 
                    nrow = dim(prices)[1],
                    byrow = TRUE)

PV_b <- matrix(rep(PV, dim(prices)[2]), 
               ncol = dim(prices)[2], 
               nrow = dim(prices)[1], 
               byrow = FALSE)

weights <- (prices*n_asset_b)/PV_b
ts_weights <- xts(weights,dates)
highchart(type = "stock") %>% 
  hc_title(text = "Portfolio weights") %>% 
  hc_add_series(ts_weights[, ticker[1]], name = ticker[1]) %>% 
  hc_add_series(ts_weights[, ticker[2]], name = ticker[2]) %>%   
  hc_add_series(ts_weights[, ticker[3]], name = ticker[3]) %>%
  hc_add_series(ts_weights[, ticker[4]], name = ticker[4]) %>%
  hc_add_series(ts_weights[, ticker[5]], name = ticker[5]) %>%
  hc_add_series(ts_weights[, ticker[6]], name = ticker[6]) %>%
  hc_add_series(ts_weights[, ticker[8]], name = ticker[8]) %>%
  hc_add_series(ts_weights[, ticker[9]], name = ticker[9]) %>%
  hc_add_series(ts_weights[, ticker[10]], name = ticker[10]) %>%
  hc_add_theme(hc_theme_flat()) %>% 
  hc_navigator(enabled = FALSE) %>% 
  hc_scrollbar(enabled = FALSE) %>% 
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = TRUE) 

# time plot of portfolio value
png("portfolio_value.png",res=320,height=210,width=297,units = "mm")
plot(PV, type = 'l',main = "Portfolio Value. January 03, 2007 - December 31, 2009")
dev.off()

### P&L ###
losses <- -diff(PV)
losses <- as.data.frame(losses)
ggplot(losses, aes(y=V1)) + 
  geom_boxplot(outlier.size=1.5,outlier.shape=21) + 
  theme_light() +
  scale_y_continuous(breaks=seq(-1000,1000,100)) + 
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(), 
        axis.ticks.x = element_blank(),
        plot.title = element_text(hjust=0.5)) +
  coord_flip() +
  ggtitle("Portfolio 'P&L' Distribution\nJan 03, 2007 - Dec 31, 2009\nPositives values are the losses")
  #ggsave("boxplot_profit_and_losses.png",dpi = 320, width=297,height = 210, device = "png",units = "mm")

#### VAR for the return distribution ####
pv_rts <- apply(PV,2,quantmod::Delt, type = 'arithmetic')
mean_pv_rts <- mean(pv_rts[-1,])
sd_log_rts <- sd(pv_rts[-1,])

#Var as a percentage of portfolio's current value
var_gauss <- qnorm(0.95)*sd_log_rts - mean_pv_rts
var_hist <- quantile(pv_rts[-1,],p=0.95)

# Difference btw parametric and non parametric var
pv_rts <- as.data.frame(-pv_rts)
colnames(pv_rts) <- "Losses"
mean_loss <- mean(pv_rts$Losses, na.rm = TRUE)
sd_loss <- sd(pv_rts$Losses, na.rm = TRUE)

dx <- density(pv_rts$Losses, na.rm = TRUE)
xnew <- 0.033
y_hist <- approx(dx$x, dx$y, xout = xnew)

df <- data.frame(x1 = 0.033, y1 = y_hist$y, 
                 x2 = 0.04, y2 = dnorm(0.04, mean = mean_loss, sd = sd_loss))

ggplot(pv_rts, aes(x = Losses)) + 
  geom_density(colour = '#525252') +
  theme_bw() +
  xlab('(%) Percentual Loss of Actual Portfolio Value: $10426.42') +
  stat_function(fun = dnorm, 
                args = list(mean = mean_loss, sd = sd_loss),
                colour = "red") +
  geom_segment(aes(x = x1, xend = x1, y = 0, yend = y1), colour = "#525252", data = df) +
  geom_segment(aes(x = x2, xend = x2, y = 0, yend = y2), colour = 'red', data = df) +
  #annotate('text', x = 0.05, y = 5, label = 'VaR 95%\nNormal', colour = 'red') +
  #annotate('text', x = 0.025, y = 5, label = 'VaR 95%\nHistorical', colour = 'black') +
  ggtitle('1-day 95% Historical VaR\nVS\n1-day 95% Normal VaR') +
  theme(plot.title = element_text(hjust = 0.5)) +
  #ggsave("historical_vs_normal_2.png",dpi = 320, width=297,height = 210, device = "png",units = "mm")

##### VAR backtest #####
dataraw_test <- read.csv("/Users/Andrea/__LSE_RISK/VaR_Backtesting.csv")
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
Daily_norm_Var <- zoo::rollapply(entire_losses_ts, width = Time_t, 
                                 align = 'right', FUN = VaR_t_Normal, alpha = 0.95)

# We shift forward by one day the series because we are calculating the VaR
# over one-day risk horizon. However, rollapply locates the first result 
# in correspondence of the time instant from which we account for the risk 
# horizon. This operation allow to make a comparison between the realized
# loss and the one-day VaR.
Daily_norm_Var <- lag(Daily_norm_Var, k = 1)

#### HISTORICAL VAR ####
Daily_hs_Var <- zoo::rollapply(entire_losses_ts, width = Time_t, align = 'right', 
                               FUN = VaR_t_hs, alpha = 0.95)
Daily_hs_Var <- lag(Daily_hs_Var, k = 1)

# We select the range to be the entire 2010 because it is our test
loss.2010 <- window(entire_losses_ts, start = as.Date('2010-01-04'), 
                    end = as.Date('2010-12-31'))
norm.Var_2010 <- window(Daily_norm_Var, start = as.Date('2010-01-04'), 
                        end = as.Date('2010-12-31'))
hs.Var_2010 <- window(Daily_hs_Var, start = as.Date('2010-01-04'), 
                      end = as.Date('2010-12-31'))

par(mfrow=c(2,1))
plot(merge(loss.2010, norm.Var_2010), main = "95% 1d VaR and P&L - Normal Method")
plot(merge(loss.2010, hs.Var_2010), main = "95% 1d VaR and P&L - Historical Method")

# We count and compare the violations
n_violations_hs <- sum(entire_losses_ts > hs.Var_2010, na.rm = TRUE)
n_violations_norm <- sum(entire_losses_ts > norm.Var_2010, na.rm = TRUE)

df_violations <- data.frame(hs = n_violations_hs, norm = n_violations_norm)
colnames(df_violations)[1] <- "Numb. of violations - Historical"
colnames(df_violations)[2] <- "Numb. of violations - Normal"




