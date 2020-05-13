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
colnames(data)[2:(nAssets+1)] <- ticker
log_rts <- apply(data, 2, quantmod::Delt, type = 'log')
fin.stocks <- as.data.frame(log_rts[,c("WFC", "NTRS", "MS", "GS")])
fin.pairs <- ggpairs(fin.stocks, lower = list(continuous = wrap("points", size = 0.1))) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        strip.text = element_text(face="bold", size=rel(0.60)),
        plot.title=element_text(size = rel(1.1), 
                                lineheight = .9, 
                                family = "Helvetica",
                                face = "plain", 
                                colour = "black",
                                hjust = 0.5)) +
  ggtitle("Pair-wise 'Financials' Stock Returns")
  ggsave("pairwise_financials.png", device = "png", dpi = 320, width=210, height = 210, units = "mm")
fin.pairs




# library(tidyverse)
# library(scales)
# library(GGally)
# #### PAIRWISE SCATTER PLOT ####
# 
# rm(list=ls(all=TRUE)) 
# 
# dataraw <- read.csv("/Users/Andrea/__LSE_RISK/Data_project_adj_2.csv")
# names(dataraw) 
# ids <- unique(dataraw[,1])
# nAssets <- length(ids)
# nObs <- nrow(dataraw)/nAssets
# data <- matrix(NA, nrow = nObs, ncol = nAssets + 1)
# data[,1] <- dataraw[dataraw[,1] == ids[1], "date"]
# for (i in 1:nAssets){
#   data[,i+1]<-dataraw[dataraw[,1] == ids[i], "PRC"]
# }
# ticker <- as.character(unique(dataraw[,"TICKER"]))
# colnames(data)[2:(nAssets+1)] <- ticker
# head(data)
# data <- apply(data, 2, quantmod::Delt, type = 'log')
# head(data)
# data <- as.data.frame(data)
# ##### ADD TITLE TO GGPAIRS PLOT!!!
# pm <- ggpairs(data, lower = list(continuous = wrap("points", size = 0.1))) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank()) +
#   ggtitle("Pair-wise scatter plots for stock return")
# 
# fin.stocks <- as.data.frame(data[,c("BA", "WFC", "NTRS", "AIG", "MS", "GS")])
# head(fin.stocks)
# class(fin.stocks)
# fin.pair <- ggpairs(fin.stocks, lower = list(continuous = wrap("points", size = 0.1))) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank()) +
#   ggtitle("Pair-wise stock returns")
# 
# 
# 
# 
# #columnLabels = c(
# #"American International Group",
# #"Amazon.com, Inc.",
# #"Bank of America Corp.",
# #"The Goldman Sachs Group, Inc.",
# #"The Coca-Cola Company",
# #"Morgan Stanley",
# #"Microsoft Corp.",
# #"Northern Trust Corp.",
# #"Wells Fargo & Co",
# #"ExxonMobil Corp."
