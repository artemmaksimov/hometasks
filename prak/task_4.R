library(xts)
library(PerformanceAnalytics)
library(quadprog)
library(fPortfolio)
library(quantmod)

asset <- read.csv("merged_price.csv")
asset[,1] <- as.Date(asset[,1],"%d/%m/%y")
asset.xts <- as.xts(cbind(asset[,2],asset[,3], asset[,4]), order.by = asset[,1])
asset.std <- scale(asset.xts)
plot(asset.std, lty=1, main="Normalized data", type="l")
head(asset)
len<-  dim(asset)[1]
r1 <- diff(log(asset[,2]))
r2 <- diff(log(asset[,3]))
r3 <- diff(log(asset[,4]))
rates.xts <- as.xts(  cbind(r1,r2, r3), order.by = asset[2:len,1])
colnames(rates.xts)<- c("SBER","MOEX", 'LUKOIL')
head(rates.xts)

s <- c()

for(i in 2:len - 1)
{
  sum <- rates.xts[i, 'SBER'] + rates.xts[i, 'MOEX']
  s <- append(s, sum)
}


beta <- CAPM.beta(rates.xts[, "LUKOIL"], s)
beta

(alpha <- CAPM.alpha(rates.xts[, "SBER"],s,Rf = 0.0725))


m <- mean(rates.xts[, "SBER"])
vol <- sd(rates.xts[, "SBER"])
alpha <- 0.95
(VaR(rates.xts[, "SBER"], p = alpha, method = "historical"))
(var1.norm <- qnorm(1 - alpha, mean = m, sd = 1))

# expected shortfall
# normal
(ES(rates.xts[, "SBER"], p = alpha, method = "gaussian"))

# historical
(ES(rates.xts[, "SBER"], p = alpha, method = "historical"))


m <- mean(rates.xts[, "MOEX"])
vol <- sd(rates.xts[, "MOEX"])
alpha <- 0.95
(VaR(rates.xts[, "MOEX"], p = alpha, method = "historical"))
(var1.norm <- qnorm(1 - alpha, mean = m, sd = 1))

# expected shortfall
# normal
(ES(rates.xts[, "MOEX"], p = alpha, method = "gaussian"))

# historical
(ES(rates.xts[, "MOEX"], p = alpha, method = "historical"))
