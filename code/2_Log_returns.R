library(tidyverse)
library(scales)

rm(list=ls(all=TRUE)) 

data <- read.csv("/Users/Andrea/__LSE_RISK/Data_project_adj_2.csv")

#### LOG RETURNS ####
Log_rets <- function(vec) {
  log_rts = diff(log(vec))
}

#### LOG RETURNS PLOT ####
### manipulating data in order to use facet_wrap function in ggplot

PCR <- as.matrix(data$PRC, ncol = 1)
Log_PCR <- as.numeric(apply(PCR, 2, Log_rets))


TICKER <- as.character(data[-1,3])
Date <- as.numeric(data[-1,2])
data_log <- as.data.frame(cbind(Date, TICKER, Log_PCR))
head(data_log)
sapply(data_log, class)
data_log <- transform(data_log, 
                             Date = as.Date(as.character(Date), "%Y%m%d"),
                             TICKER = factor(TICKER),
                             Log_PCR = as.numeric(levels(Log_PCR))[Log_PCR])

head(data_log)
dt_leh = as.Date(as.character(rep(20080915, 10)), "%Y%m%d")
leh_collapse <- data.frame(dt_leh = dt_leh)

dt_ber = as.Date(as.character(rep(20080314, 10)), "%Y%m%d")
bear_sterns <- data.frame(dt_ber = dt_ber)

dt_october <- as.Date(as.character(rep(20081007, 10)), "%Y%m%d")
october <- data.frame(dt_october = dt_october)

levels(data_log$TICKER)[levels(data_log$TICKER) == "AIG"]  <- "American International Group"
levels(data_log$TICKER)[levels(data_log$TICKER) == "AMZN"]  <- "Amazon.com, Inc."
levels(data_log$TICKER)[levels(data_log$TICKER) == "BA"]  <- "Bank of America Corp."
levels(data_log$TICKER)[levels(data_log$TICKER) == "GS"]  <- "The Goldman Sachs Group, Inc."
levels(data_log$TICKER)[levels(data_log$TICKER) == "KO"]  <- "The Coca-Cola Company"
levels(data_log$TICKER)[levels(data_log$TICKER) == "MS"]  <- "Morgan Stanley"
levels(data_log$TICKER)[levels(data_log$TICKER) == "MSFT"]  <- "Microsoft Corp."
levels(data_log$TICKER)[levels(data_log$TICKER) == "NTRS"]  <- "Northern Trust Corp."
levels(data_log$TICKER)[levels(data_log$TICKER) == "WFC"]  <- "Wells Fargo & Co"
levels(data_log$TICKER)[levels(data_log$TICKER) == "XOM"]  <- "ExxonMobil Corp."

datebreaks <- seq(as.Date("2007-01-01"), as.Date("2009-12-31"), by="6 month")

ggplot(data_log, aes(x = Date, y = Log_PCR)) + 
  geom_line(size = 0.15) +
  facet_wrap(~ TICKER, ncol = 2, nrow = 5) +
  theme_bw() +
  theme(strip.text = element_text(face="bold", size=rel(0.60)),
        strip.background = element_rect(fill = "#F2F4F6"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title=element_text(size = rel(1.1), lineheight = .9, 
                                family = "Helvetica",
                                face = "plain", 
                                colour = "black",hjust = 0.5)) +
  scale_x_date(breaks = datebreaks, labels = date_format("%b %y")) +
  scale_y_continuous(name = 'Log Returns', limits = c(min(data_log$Log_PCR), 0.75)) +
  geom_vline(aes(xintercept = dt_leh), leh_collapse,
             linetype = 2, colour = 'red', size = 0.25) +
  geom_vline(aes(xintercept = dt_ber), bear_sterns,
             linetype = 2, colour = 'blue', size = 0.25) +
  #geom_vline(aes(xintercept = dt_october), october,
             #linetype = 2, colour = 'black', size = 0.25) +
  coord_cartesian(xlim = c(as.Date(as.character(20070101), "%Y%m%d"),
                           as.Date(as.character(20091231), "%Y%m%d"))) +
  ggtitle("Stock Performance Based on Log Returns\nJanuary 03, 2007 - December 31, 2009\nBear Sterns rescue in blue; Lehman's bankruptcy in red") +
  ggsave("log_returns.png", device = "png", dpi = 320, width=210, height = 297, units = "mm")
  
  #ggtitle("Stock Performance Based on Log Returns\nJanuary 03, 2007 - December 31, 2009\nBear Sterns rescue in blue; Lehman's bankruptcy in red")

pair_normal_test <-  MVN::mvn(data = log_rts[,c(2,3)], mvnTest = "mardia", multivariatePlot = "persp" )
pair_normal_test$multivariateNormality









