library(tidyverse)
library(scales)
rm(list=ls(all=TRUE)) 
setwd("/Users/Andrea/__LSE_RISK/images")
dataraw <- read.csv("/Users/Andrea/__LSE_RISK/Data_project_adj_2.csv")
dim(dataraw)
#### PRICE DYNAMICS PLOT ####
dataraw <- dataraw[,-c(1,5)]
dataraw <- transform(dataraw, Date = as.Date(as.character(date), "%Y%m%d"))

dt_leh <-  as.Date(as.character(rep(20080915, 10)), "%Y%m%d")
leh_collapse <- data.frame(dt_leh = dt_leh)
dt_ber = as.Date(as.character(rep(20080314, 10)), "%Y%m%d")
bear_sterns <- data.frame(dt_ber = dt_ber)
dt_october <- as.Date(as.character(rep(20081007, 10)), "%Y%m%d")
october <- data.frame(dt_october = dt_october)
datebreaks <- seq(as.Date("2007-01-01"), as.Date("2009-12-31"), by="6 month")



levels(dataraw$TICKER)[levels(dataraw$TICKER) == "AIG"]  <- "American International Group"
levels(dataraw$TICKER)[levels(dataraw$TICKER) == "AMZN"]  <- "Amazon.com, Inc."
levels(dataraw$TICKER)[levels(dataraw$TICKER) == "BA"]  <- "Bank of America Corp."
levels(dataraw$TICKER)[levels(dataraw$TICKER) == "GS"]  <- "Goldman Sachs Group, Inc."
levels(dataraw$TICKER)[levels(dataraw$TICKER) == "KO"]  <- "Coca-Cola Company"
levels(dataraw$TICKER)[levels(dataraw$TICKER) == "MS"]  <- "Morgan Stanley"
levels(dataraw$TICKER)[levels(dataraw$TICKER) == "MSFT"]  <- "Microsoft Corp."
levels(dataraw$TICKER)[levels(dataraw$TICKER) == "NTRS"]  <- "Northern Trust Corp."
levels(dataraw$TICKER)[levels(dataraw$TICKER) == "WFC"]  <- "Wells Fargo & Co"
levels(dataraw$TICKER)[levels(dataraw$TICKER) == "XOM"]  <- "ExxonMobil Corp."

datebreaks <- seq(as.Date("2007-01-01"), as.Date("2009-12-31"), by="6 month")

ggplot(dataraw, aes(x = Date, y = PRC)) + 
  geom_line(size = 0.15) +
  facet_wrap(~ TICKER, ncol = 2, nrow = 5) +
  theme_bw() +
  theme(strip.text = element_text(face="bold", size=rel(0.60)), 
        strip.background = element_rect(fill = "#eeeeee"), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_blank(),
        plot.title=element_text(size = rel(1.1), lineheight = .9, 
                                family = "Helvetica", face = "plain", 
                                colour = "black",hjust = 0.5)) +
  scale_x_date(breaks = datebreaks, labels = date_format("%b %y")) +
  scale_y_continuous(name = 'Price ($)') +
  geom_vline(aes(xintercept = dt_leh), leh_collapse, linetype = 2, colour = 'red', size = 0.25) +
  geom_vline(aes(xintercept = dt_ber), bear_sterns, linetype = 2, colour = 'blue', size = 0.25) +
  #geom_vline(aes(xintercept = dt_october), october, linetype = 2, colour = 'black', size = 0.25) +
  coord_cartesian(xlim = c(as.Date(as.character(20070101), "%Y%m%d"),
                           as.Date(as.character(20091231), "%Y%m%d"))) +
  ggtitle("Stock Price Dynamics\nJanuary 03, 2007 - December 31, 2009\nBear Stearns rescue in blue; Lehman's bankruptcy in red") +
  ggsave("price_dynamics.png", device = "png", dpi = 320, width=210, height = 297, units = "mm")

#### LOG PRICES PLOT ####

ggplot(dataraw, aes(x = Date, y = log(PRC))) + 
  geom_line(size = 0.20) +
  facet_wrap(~ TICKER, ncol = 2, nrow = 5) +
  theme_bw() +
  theme(strip.text = element_text(face="bold", size=rel(0.60)),
        strip.background = element_rect(fill = "#eeeeee"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title=element_text(size = rel(1.1), 
                                lineheight = .9, 
                                family = "Helvetica",
                                face = "plain", 
                                colour = "black",
                                hjust = 0.5)) +
  scale_x_date(breaks = datebreaks, 
               labels = date_format("%b %y")) +
  scale_y_continuous(name = 'Log Price ($)') +
  geom_vline(aes(xintercept = dt_leh), 
             leh_collapse,
             linetype = 2, colour = 'red', size = 0.25) +
  geom_vline(aes(xintercept = dt_ber), 
             bear_sterns,
             linetype = 2, colour = 'blue', size = 0.25) +
  coord_cartesian(xlim = c(as.Date(as.character(20070101), "%Y%m%d"),
                           as.Date(as.character(20091231), "%Y%m%d"))) +
  ggtitle("Log Price Dynamics\nJanuary 03, 2007 - December 31, 2009\nBlue line: Bear Stern get Bailout; Red line: Lehman's bankruptcy")
