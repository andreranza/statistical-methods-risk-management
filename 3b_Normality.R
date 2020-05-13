# Data pre-processing
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
log_rts <- apply(data, 2, quantmod::Delt, type = 'log')
log_rts <- log_rts[-1,]

# Test for marginal (Shapiro - Wilk & Jarque-Bera) and multivariate normality (Mardia)

ppi <- 320
png("normality.png", width=10*ppi,height=10*ppi,res=ppi)
normality_test <- MVN::mvn(data = log_rts[,-1], mvnTest = "mardia", univariatePlot = "qqplot")
dev.off()


normality_test$Descriptives
normality_test$univariateNormality
normality_test$multivariateNormality

# Jarque - Bera test
JBTest <- function(dataMatrix){
  ncol <- dim(dataMatrix)[2]
  pvals <- array(NA, ncol)
  for (i in 1:ncol) pvals[i] <- tseries::jarque.bera.test(dataMatrix[,i])$p.value
  if (!is.null(names(dataMatrix))) names(pvals) <- names(dataMatrix)
  return(pvals)
}

JBTest(log_rts[,-1])
