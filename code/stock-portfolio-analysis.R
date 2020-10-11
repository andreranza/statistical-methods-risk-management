## ----packages------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(tidyquant)
library(lubridate)
library(broom)
library(scales)
library(knitr)
library(broom)
library(qqplotr)
library(scales)

colours <- c("#7cb5ec", "#434348", "#90ed7d", "#f7a35c", "#8085e9", 
             "#f15c80", "#e4d354", "#2b908f", "#f45b5b", "#91e8e1")

## ----dataraw-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataraw <- read_csv("data/stock-prices.csv") %>% 
  select(-c("PERMNO","VOL")) %>% 
  mutate(DATE = ymd(date),
         TICKER = factor(TICKER)) %>% 
  select(-date) %>% 
  relocate(DATE, .before = everything()) %>% 
  mutate(Sector = case_when(
    TICKER %in% c("AIG","BA","GS","MS","NTRS","WFC") ~ "Financial",
    TICKER %in% c("AMZN","KO") ~ "Consumer",
    TICKER == "XOM" ~ "Energy",
    TICKER == "MSFT" ~ "IT")) %>% 
  mutate(Sector = factor(Sector))


## ----percentage-price-change---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataraw %>% 
  filter(DATE %in% c(ymd("2008-03-14"), ymd("2009-01-20"))) %>% 
  pivot_wider(names_from = DATE, values_from = PRC) %>% 
  rename(P2 = "2009-01-20") %>%
  rename(P1 = "2008-03-14") %>%
  mutate(DELTA = ((P2-P1)/P1)*100)

## ----percentage-price-change-graph---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataraw %>% 
  filter(DATE %in% c(ymd("2008-03-14"), ymd("2009-01-20"))) %>% 
  arrange(DATE) %>% 
  mutate(DATE = factor(DATE)) %>% 
  ggplot(aes(DATE, PRC, group = TICKER)) +
  geom_line(aes(color = Sector), size = 1.05) +
  geom_point(aes(color = Sector), size = 1.25) +
  scale_fill_brewer() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "top") +
  labs(y = "Price ($)",
       x = "Days",
       title = "Change in Prices from March 14, 2008 to January 20, 2009",
       caption = "Source: Wharton Research Data Services") 


## ----price-dynamics------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(dataraw, aes(x = DATE, y = PRC, group = TICKER, colour = TICKER)) + 
  geom_line(size = 0.5) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top") +
  # Lehman bankruptcy
  geom_vline(xintercept = ymd("2008-09-15"), colour = "#696969", 
             size = 0.5, linetype = 2) +
  # Bear Stearns rescue
  geom_vline(xintercept = ymd("2008-03-14"), colour = "#696969", 
             size = 0.5, linetype = 2) +
  labs(title = "Stock Price Dynamics",
       subtitle = "January 03, 2007 - December 31, 2009\nBear Stearns rescue; Lehman's bankruptcy",
       caption = "Source: Wharton Research Data Services",
       x = "Date",
       y = "Price") +
  scale_colour_manual(values = colours)


## ----log-returns---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logReturns <- dataraw %>% 
  select(-Sector) %>% 
  pivot_wider(names_from = TICKER, values_from = PRC) %>% 
  mutate(across(where(is.numeric), ~ log(.x))) %>% 
  mutate(across(where(is.numeric), ~ .x - lag(.x, n = 1))) %>% 
  slice(-1) 


## ----log-returns-graph, fig.width=1.41*6,fig.height=8--------------------------------------------------------------------------------------------------------------------------------------------------------------
logReturns %>% 
  pivot_longer(cols = !DATE, names_to = "TICKER", values_to = "LOGPRICE") %>% 
  arrange(TICKER) %>% 
  ggplot(aes(x = DATE, y = LOGPRICE, colour = TICKER)) +
  geom_line(size = 0.35) + 
  facet_wrap(~ TICKER, ncol = 2, nrow = 5) +
  theme_light() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(-1,1)) +
  scale_colour_manual(values = colours, guide = FALSE) +
  # Lehman bankruptcy
  geom_vline(xintercept = ymd("2008-09-15"), colour = "#696969", 
             size = 0.5, linetype = 2) +
  # Bear Stearns rescue
  geom_vline(xintercept = ymd("2008-03-14"), colour = "#696969", 
             size = 0.5, linetype = 2) +
  labs(y = "Log Prices",
       x = "Date",
       title = "Stock Performance Based on Log Returns",
       subtitle = "January 03, 2007 - December 31, 2009",
       caption = "Source: Wharton Research Data Services")



## ----jarque-bera-test----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
logReturns %>% 
  pivot_longer(cols = !DATE, names_to = "TICKER", values_to = "PRICE") %>% 
  select(-DATE) %>% 
  nest(ts = c(PRICE)) %>% 
  mutate(ts = map(ts, ~ as.matrix(.x))) %>% 
  mutate(ts = map(ts, ~ tseries::jarque.bera.test(.x))) %>% 
  mutate(ts = map(ts, tidy)) %>% 
  unnest(ts) %>% 
  mutate(Normal = "No") %>% 
  select(-c(parameter,method)) %>% 
  arrange(statistic)

## ----marginal-normality-QQplot, fig.show="hold", results='hide', fig.height=18-------------------------------------------------------------------------------------------------------------------------------------
logReturnsList <- logReturns  %>% 
  select(!DATE) %>% 
  pivot_longer(cols = everything(), names_to = "TICKER", values_to = "PRC") %>% 
  group_by(TICKER) %>% 
  nest() %>% 
  mutate(data = map(data, ~ as.data.frame(.x))) %>% 
  pull(data) %>% 
  set_names(nm = unique(dataraw$TICKER))

tickers <- names(logReturnsList)

qqPlots <- vector(mode = "list", length = length(logReturnsList))
for (i in seq_along(logReturnsList)) {
  qqPlots[[i]] <- ggplot(logReturnsList[[i]], aes(sample = PRC)) +
    qqplotr::stat_qq_line(colour = "black", size = 0.7) +
    qqplotr::stat_qq_point(size = 0.6, colour = "#696969") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
         title = str_c("Stock: ", tickers[i])) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
}

gridExtra::grid.arrange(grobs = qqPlots, nrow = 5)

## ----student-t-test------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# One-sample Kolmogorov-Smirnov test for student-t distribution
logReturns %>% 
  select(!DATE) %>% 
  as.list() %>% 
  map(~ ks.test(.x, "pt", df = 10)) %>% 
  enframe() %>% 
  mutate(value = map(value, ~ tidy(.x))) %>% 
  unnest(value) %>% 
  select(-method) %>% 
  arrange(statistic) %>% 
  mutate(t.Student = rep("NO", 10))


## ----marginal-t-student-QQplot, fig.show="hold", results='hide', fig.height=18-------------------------------------------------------------------------------------------------------------------------------------
qqPlots2 <- vector(mode = "list", length = length(logReturnsList))
for (i in seq_along(logReturnsList)) {
  qqPlots2[[i]] <- ggplot(logReturnsList[[i]], aes(sample = PRC)) +
    qqplotr::stat_qq_line(distribution = "t", dparams = 10, colour = "black", size = 0.7) +
    qqplotr::stat_qq_point(size = 0.6, colour = "#696969") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles",
         title = str_c("Stock: ", tickers[i])) +
    theme_light() +
    theme(plot.title = element_text(hjust = 0.5))
}
gridExtra::grid.arrange(grobs = qqPlots2, nrow = 5)


## ----log-returns-kurtosis-skewness---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dataraw %>% 
  select(-Sector) %>% 
  pivot_wider(names_from = TICKER, values_from = PRC) %>% 
  mutate(across(where(is.numeric), ~ log(.x))) %>% 
  mutate(across(where(is.numeric), ~ .x - lag(.x, n = 1))) %>% 
  slice(-1) %>% 
  select(!DATE) %>% 
  MVN::mvn(mvnTest = "mardia") %>% 
  pluck(3) %>% 
  select(Skew, Kurtosis) %>% 
  arrange(Kurtosis)


## ----multivariate-normality----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
normality_test <- dataraw %>% 
  select(-Sector) %>% 
  pivot_wider(names_from = TICKER, values_from = PRC) %>% 
  mutate(across(where(is.numeric), ~ log(.x))) %>% 
  mutate(across(where(is.numeric), ~ .x - lag(.x, n = 1))) %>% 
  slice(-1) %>% 
  select(!DATE) %>% 
  MVN::mvn(mvnTest = "mardia")

normality_test$multivariateNormality %>% 
  slice(1:2) %>% 
  mutate(Statistic = as.double(levels(Statistic))) 


## ----multivariate-normality-graph----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
normality_test <- dataraw %>% 
  select(-Sector) %>% 
  pivot_wider(names_from = TICKER, values_from = PRC) %>% 
  mutate(across(where(is.numeric), ~ log(.x))) %>% 
  mutate(across(where(is.numeric), ~ .x - lag(.x, n = 1))) %>% 
  slice(-1) %>% 
  select(!DATE) %>% 
  MVN::mvn(mvnTest = "mardia", multivariatePlot = "qq")


## ----financial-stocks----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
finStocks <- dataraw %>% 
  filter(Sector == "Financial") %>% 
  select(-Sector) %>% 
  pivot_wider(names_from = "TICKER", values_from = "PRC") %>% 
  mutate(across(where(is.numeric), ~ log(.x))) %>% 
  mutate(across(where(is.numeric), ~ .x - lag(.x, n = 1))) %>% 
  slice(-1) %>% 
  select(-DATE) %>% 
  as.matrix()


## ----pseudo-observations, fig.dim=c(10,10)-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#library(QRM)
library(copula)
pseudos <- copula::pobs(finStocks)

# Equivalently to find pseudo-observations
# as_tibble(finStocks) %>% 
#   mutate(across(everything(), ~ QRM::edf(.x, adjust = 1)))


## ----fit-multivariate-normal-copula--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
normalCopula <- fitCopula(normalCopula(dim = ncol(finStocks), dispstr = "un"), 
                           data = pseudos, 
                           method = "mpl")
logLikNormal <- summary(normalCopula)$loglik


## ----fit_multivariate-student-t------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tStCopula <- fitCopula(tCopula(dim = ncol(finStocks), dispstr = "un"),
                       data = pseudos,
                       method  = "mpl")
logLikStudent <- summary(tStCopula)$loglik


## ----fit-multivariate-gumbel---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gumbCompula <- fitCopula(gumbelCopula(dim = ncol(finStocks)), 
                                      data = pseudos,
                         method = "mpl")
logLikGumbel <- summary(gumbCompula)$loglik


## ----fit-multivariate-clayton--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
clayCompula <- fitCopula(gumbelCopula(dim = ncol(finStocks)), 
                                      data = pseudos,
                         method = "mpl")
logLikClayton <- summary(clayCompula)$loglik


## ----pairwise-scatter-financials, fig.dim=c(10,10)-----------------------------------------------------------------------------------------------------------------------------------------------------------------
logReturns %>% 
  select(c("AIG","BA","GS","MS","NTRS","WFC")) %>%
  GGally::ggpairs(lower = list(continuous = GGally::wrap("points", size = 0.1))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        strip.text = element_text(face="bold", size=rel(0.60)),
        plot.title=element_text(size = rel(1.1), 
                                lineheight = .9, 
                                family = "Helvetica",
                                face = "plain", 
                                colour = "black",
                                hjust = 0.5)) +
  labs(title = "Pair-wise Returns scatter plots of Financial Stocks")


## ----pairwise-financial-mardia-test--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
combs <- t(combn(1:ncol(finStocks), m = 2))

combsName <- c()
values <- vector(mode="list", length = nrow(combs))

for (i in 1:nrow(combs)) {
  combsName[i] <- str_c(colnames(finStocks[,combs[i, ]]), collapse = "-")
  values[[i]] <- MVN::mvn(finStocks[,combs[i, ]], 
                          mvnTest = "mardia")$multivariateNormality
}

values %>% 
  set_names(nm = combsName) %>% 
  enframe() %>% 
  mutate(value = map(value, ~ as_tibble(.x))) %>% 
  unnest(value) %>% 
  filter(Test != "MVN") %>% 
  mutate(across(where(is.factor), ~ as.character(.x))) %>% 
  mutate(Statistic = as.numeric(Statistic)) %>% 
  rename(P.value = "p value") %>% 
  mutate(P.value = as.numeric(P.value)) %>% 
  group_by(name) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(data = map2(data, combsName, 
                   ~ kable(.x, caption = str_c("Normality test between: ", .y),
                           digits = 2, 
                           format.args = list(big.mark = ".", 
                                              decimal.mark = ",", 
                                              scientific = FALSE)))) %>% 
  pull(data) %>% 
  kables()


## ----empirical-scatter-pseudo-observations, fig.dim=c(8,8), fig.cap="Figure (a): Pseudo observations (uniform margins) obtained from actual data, with non parametric estimation of the CDFs."---------------------
splom2(pseudos, cex = 0.2, col.mat = "black", pch = 16)


## ----fit-bivariate-t-copula----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bivStudCopula <- vector(mode = "list", length = nrow(combs))
for (i in 1:nrow(combs)) {
  bivStudCopula[[i]] <- fitCopula(tCopula(dim = 2, dispstr = "un"), 
                                  data = pseudos[, combs[i,]],
                                  method = "mpl")
}
names(bivStudCopula) <- combsName


## ----multivariate-student-t-simulation, fig.dim=c(8,8), fg.cap="Figure (b): 755 simulated pseudo-observations obtained from a multivariate Student-t distribution with 4 dof and equicorrelation dispersion matrix with rho = 0.7"----
nObs <- nrow(finStocks)
tNVar <- ncol(finStocks)
cor.par <- 0.7

# Dispersion matrix
tSigma <- (1-cor.par)*diag(tNVar) + matrix(cor.par, c(tNVar,tNVar), nrow = 6)
t.dof <- 4

# Multivariate dim 6 student t distribution (margins are uniform)
tData <- mnormt::rmt(nObs, mean = rep(0, tNVar), S = tSigma, df = t.dof)
tCopulaData <- apply(tData, 2, pt, df = t.dof)
splom2(tCopulaData, cex = 0.2, col.mat = "black", pch = 16)

# Contour charts
distribution.limits <- c(0.01, 0.99)
Sigma.contour <- QRM::equicorr(2, 0.5)
threed.limits <- c(-3, 3)

par(mfrow = c(1,2))
QRM::BiDensPlot(func = QRM::dcopula.t, xpts = distribution.limits, ypts = distribution.limits,
           df = t.dof, Sigma = Sigma.contour, type = "contour")

QRM::BiDensPlot(func = QRM::dcopula.t, xpts = distribution.limits , 
                ypts = distribution.limits, df = t.dof, Sigma=Sigma.contour, 
                type = "persp", npts =15)



## ----summary-log-likelihood----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
loglikelihoods <- tibble(logLikNormal,logLikStudent,logLikGumbel,logLikClayton)
kable(loglikelihoods, 
      caption = "Table: MLE results of four different multivariate copula of the Financial stocks",
      digits = 2, 
      format.args = list(big.mark = ".", decimal.mark = ",", scientific = FALSE),
      col.names = c("Normal","Student","Gumbel","Clayton"))


## ----histograms-pseudo-obs, results="hide", fig.height=8, fig.cap="Figure: Pseudo-observations Distribution"-------------------------------------------------------------------------------------------------------
finNames <- sort(c("AIG","BA","GS","MS","NTRS","WFC"))

uniHist <- pobs(finStocks) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything()) %>% 
  arrange(name) %>% 
  nest(value) %>% 
  #pull(data) %>% 
  mutate(plots = map2(data, finNames, ~ ggplot(.x, aes(x = value)) + 
                        geom_histogram(colour = "grey", 
                                       fill = "lightblue",
                                       bins = 30) +
                        theme_light() +
                        theme(plot.title = element_text(hjust = 0.5)) +
                        labs(title = str_c("Stock: ", .y)))) %>% 
  pull(plots)
gridExtra::grid.arrange(grobs = uniHist, ncol = 2)


## ----fit-bivariate-normal------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bivNormalCopula <- vector(mode="list", length = nrow(combs))
for (i in 1:nrow(combs)) {
  bivNormalCopula[[i]] <- fitCopula(normalCopula(dim = 2), 
                                    data = pseudos[,combs[i,]],
                                    method = "mpl")
}
names(bivNormalCopula) <- combsName


## ----fit-bivariate-gumbel------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bivGumbelCopula <- vector(mode = "list", length = nrow(combs))
for (i in 1:nrow(combs)) {
  bivGumbelCopula[[i]] <- fitCopula(normalCopula(dim = 2), 
                                    data = pseudos[,combs[i,]],
                                    method = "mpl")
}
names(bivGumbelCopula) <- combsName


## ----fit-bivariate-clayton, eval=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## # bivClaytonCopula <- vector(mode = "list", length = nrow(combs))
## # for (i in 1:nrow(combs)) {
## #   bivClaytonCopula[[i]] <- fitCopula(claytonCopula(dim = 2),
## #                                      data = pseudos[,combs[i,]],
## #                                      method = "mpl")
## # }
## # names(bivClaytonCopula) <- combsName


## ----portfolio-value-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# The value of the portfolio at the beginning of the period is $10.000
prices <- dataraw %>% 
  select(-Sector) %>% 
  pivot_wider(names_from = TICKER, values_from = PRC) %>% 
  select(-DATE)

prices0 <- slice(prices, 1) %>% 
  select(everything()) %>% 
  unlist()

capital0 <- rep(1000, ncol(prices))
names(capital0) <- colnames(prices)
pfValue0 <- sum(capital0)

# Number of stocks purchased at time t=0 is kept fixed along successive time 
# periods
nAssets0 <- capital0/prices0
names(nAssets0) <- str_c("n.", colnames(prices))

# Portfolio value
PV <- as.matrix(prices)%*%as.matrix(nAssets0, nrow = nrow(prices))
PV %>% 
  as_tibble_col(column_name = "PV") %>% 
  mutate(DATE = unique(dataraw$DATE)) %>% 
  relocate(DATE, .before = everything()) %>% 
  ggplot(aes(DATE, PV)) +
  geom_line() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
    # Lehman bankruptcy
  geom_vline(xintercept = ymd("2008-09-15"), colour = "#696969", 
             size = 0.5, linetype = 2) +
  # Bear Stearns rescue
  geom_vline(xintercept = ymd("2008-03-14"), colour = "#696969", 
             size = 0.5, linetype = 2) +
  labs(title = "Portfolio value",
       subtitle = "Inital number of stocks kept fixed for the entire time framework.\nJanuary 03, 2007 - December 31, 2009",
       caption = "Source: Wharton Research Data Services",
       x = "Date",
       y = "($)")

## ----portfolio-weights---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Find the weights of each stock at time t by dividinding its values by the 
# value of the portfolio at time t
PV <- PV %>% 
  as_tibble() %>% 
  mutate(DATE = unique(dataraw$DATE)) %>% 
  relocate(DATE, before = everything())

prices <- prices %>% 
  mutate(DATE = unique(dataraw$DATE)) %>% 
  relocate(DATE, before = everything()) 

nAssets0 <- as_tibble(nAssets0)

left_join(prices, PV) %>% 
  rename(PV = V1) %>% 
  relocate(PV, .after = DATE) %>% 
  pivot_longer(cols = !c(DATE,PV), names_to = "TICKER", values_to = "PRC") %>% 
  mutate(n = unlist(rep(nAssets0, nrow(prices)))) %>% 
  group_by(TICKER) %>% 
  nest() %>% 
  mutate(data = map(data, ~ mutate(.x, w = (PRC*n)/PV))) %>% 
  unnest() %>% 
  select(TICKER, DATE, w) %>% 
  ggplot(aes(x = DATE, y = w, colour = TICKER)) +
  geom_line() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top") +
  labs(title = "Portfolio Weights Dynamics",
       subtitle = "Inital number of stocks kept fixed for the entire time framework.\nJanuary 03, 2007 - December 31, 2009",
       caption = "Source: Wharton Research Data Services",
       x = "Date",
       y = "Weight") +
  scale_colour_manual(values = colours)



## ----portfolio-profit-loss, fig.height=10--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
gg1 <- PV %>% 
  mutate(PL = -(V1 - lag(V1, n = 1))) %>% 
  ggplot(aes(x = DATE, y = PL)) +
  geom_line(size = 0.35) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  # Lehman bankruptcy
  geom_vline(xintercept = ymd("2008-09-15"), colour = "red", 
             size = 0.5, linetype = 2) +
  # Bear Stearns rescue
  geom_vline(xintercept = ymd("2008-03-14"), colour = "red", 
             size = 0.5, linetype = 2) +
  labs(title = "Absolute Portfolio Profit and Loss",
       subtitle = "January 03, 2007 - December 31, 2009",
       caption = "Source: Wharton Research Data Services",
       y = "p&l",
       x = "Date") 

gg2 <- PV %>% 
  mutate(PL = -(V1 - lag(V1, n = 1))) %>% 
  ggplot(aes(x = PL)) +
  geom_histogram(fill = "lightblue", colour = "grey", bins = 40) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Absolute Portfolio Profit and Loss",
       subtitle = "January 03, 2007 - December 31, 2009",
       caption = "Source: Wharton Research Data Services",
       x = "p&l") 

gg3 <- PV %>% 
  mutate(PL = -(V1 - lag(V1, n = 1))) %>% 
  ggplot(aes(x = PL)) +
  geom_boxplot() +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Absolute Portfolio Profit and Loss",
       subtitle = "January 03, 2007 - December 31, 2009",
       caption = "Source: Wharton Research Data Services",
       x = "p&l") 

plPlots <- list(gg1, gg2, gg3)

gridExtra::grid.arrange(grobs = plPlots, ncol = 1)


## ----VAR-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
PL <- PV %>% 
  mutate(PL = -((V1 - lag(V1, n = 1))/V1)) %>% 
  pull(PL)
  
meanPL <- PV %>% 
  mutate(PL = (V1 - lag(V1, n = 1))/V1) %>% 
  summarise(mean = mean(PL, na.rm = TRUE)) %>% 
  pull()

sdPL <- PV %>% 
  mutate(PL = (V1 - lag(V1, n = 1))/V1) %>% 
  summarise(sd = sd(PL, na.rm = TRUE)) %>% 
  pull()

# Var as a percentage of portfolio’s current value
varGauss <- qnorm(0.95)*sdPL - meanPL
names(varGauss) <- "95%"
varHistorical <- quantile(PL, p = 0.95, na.rm = TRUE)


percPL <- PV %>% 
  mutate(percPL = -((V1 - lag(V1, n = 1))/V1)) %>% 
  slice(-1)

meanPercPl <- percPL %>% 
  summarise(mean = mean(percPL)) %>% 
  pull()

sdPercPl <- percPL %>% 
  summarise(sd = sd(percPL)) %>% 
  pull()

densSupport <- density(percPL$percPL)
yKernel <- approx(densSupport$x, densSupport$y, xout = varHistorical)$y

segmentsCoord <- data.frame(x1 = varHistorical, y1 = yKernel,
                            x2 = varGauss, y2 = dnorm(varGauss, 
                                                      mean = meanPercPl,
                                                      sd = sdPercPl))
percPL %>% 
  ggplot(aes(x = percPL)) +
  geom_density(colour = "black") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggplot2::stat_function(fun = dnorm, 
                         args = list(mean = meanPercPl, sd = sdPercPl),
                         colour = "red") +
  geom_segment(aes(x = x1, xend = x1, y = 0, yend = y1), data = segmentsCoord) +
  geom_segment(aes(x = x2, xend = x2, y = 0, yend = y2), 
               colour = 'red', data = segmentsCoord) +
  labs(title = "Historical and Normal Value-at-Risk (one day horizon)",
       subtitle = "January 03, 2007 - December 31, 2009",
       caption = "Source: Wharton Research Data Services",
       x = "p&l")


## ----var-backtesting-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Contains the 2010 financial year
varBacktesting <- read_csv("data/var-backtesting.csv")

entireSerie <- bind_rows(dataraw, varBacktesting) %>% 
  select(-Sector) %>% 
  pivot_wider(names_from = TICKER, values_from = PRC)
  
PVentire <- as.matrix(entireSerie[,-1])%*%as.matrix(nAssets0, 
                                                    nrow = ncol(entireSerie))

PLentire <- PVentire %>% 
  as_tibble() %>% 
  mutate(absPL = -((value - lag(value, n = 1))/value)) %>% 
  mutate(DATE = entireSerie$DATE) %>% 
  relocate(DATE, before = everything())

valueAtRiskGauss <- function(vec, alpha) {
  muHat = mean(vec, na.rm = TRUE)
  sdHat = sd(vec, na.rm = TRUE)
  VaR = muHat + sdHat * qnorm(alpha)
  return(VaR)
}

valueAtRiskHistorical <- function(vec, alpha) {
  VaR = quantile(vec, alpha, na.rm = TRUE)
  return(VaR)
}

rollwindowSize <- (nrow(dataraw)/nrow(nAssets0))

# First computed VaR is on December 31, 2009 (one day horizon).
# We shift forward by one day the series because we are calculating the VaR
# over one-day risk horizon. In fact, "rollapply" locates the first result
# in correspondence of the time instant from which we account for the risk
# horizon. This operation allows to make a comparison between the realized
# loss and the one-day VaR.
#### Rolling Gaussian Value at Risk
rollGauss <- PLentire %>% 
  select(-value) %>% 
  tidyquant::tq_mutate(mutate_fun = rollapply,
                       align = "right",
                       width = rollwindowSize,
                       FUN = valueAtRiskGauss,
                       alpha = 0.95,
                       by.column = FALSE) %>% 
  filter(!is.na(value)) %>% 
  # Move ahead in order to compare actual loss with VaR
  mutate(value = lag(value, n = 1)) %>% 
  mutate(Violation = if_else(absPL > value, "YES", "NO"))

numberViolationG <- rollGauss %>% 
  slice(-1) %>% 
  count(Violation)

#### Rolling Hstorical Value at Risk
rollHist <- PLentire %>% 
  select(-value) %>% 
  tidyquant::tq_mutate(mutate_fun = rollapply,
                       align = "right",
                       width = rollwindowSize,
                       FUN = valueAtRiskHistorical,
                       alpha = 0.95,
                       by.column = FALSE) %>% 
  filter(!is.na(value)) %>% 
  # Move ahead in order to compare actual loss with VaR
  mutate(value = lag(value, n = 1)) %>% 
  mutate(Violation = if_else(absPL > value, "YES", "NO"))

numberViolationH <- rollHist %>% 
  slice(-1) %>% 
  count(Violation)

# Graph violations
viol1 <- rollGauss %>% 
  slice(-1) %>% 
  mutate(absPL = absPL) %>% 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = absPL)) +
  geom_line(aes(y = value, colour = "red")) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(subtitle = "95% one-day horizon Gaussian Value-at-Risk of the portfolio",
       title = "VaR backtest",
       caption = "Source: Wharton Research Data Services",
       x = "Date",
       y = "p&l")

viol2 <- rollHist %>% 
  slice(-1) %>% 
  mutate(absPL = absPL) %>% 
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = absPL)) +
  geom_line(aes(y = value, colour = "red")) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none") +
  labs(subtitle = "95% one-day horizon Historical Value-at-Risk of the portfolio",
       title = "VaR backtest",
       caption = "Source: Wharton Research Data Services",
       x = "Date",
       y = "p&l")

violationsPlots <- list(viol1, viol2)

gridExtra::grid.arrange(grobs = violationsPlots, ncol = 1)

