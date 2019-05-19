asset <- read.csv('./Documents/prak/notebooks/task_2/merged_price.csv')
head(asset)

library(xts)
library(PerformanceAnalytics)
library(quadprog)
library(fPortfolio)
library(quantmod)

#scaled
asset[,1] <- as.Date(asset[,1],"%Y-%m-%d")
asset.xts <- as.xts(cbind(asset[,2],asset[,3], asset[,4], asset[,5], asset[,6], asset[,7], asset[,8]), order.by = asset[,1])
asset.std <- scale(asset.xts)
plot(asset.std, lty=1, main="Normalized data", type="l")

#geom returns
yield <- returns(asset.xts, method="discrete")
yield
colnames(yield) <- colnames(asset)[2:8]
yield <- yield[2 : nrow(yield),]
yield.std <- returns(asset.std, method="discrete")
colnames(yield.std) <- colnames(asset)[2:8]
yield.std <- yield.std[2 : nrow(yield.std),]

# covariance matrix
(cov <- cov(yield))
n <- ncol(cov)
Amat <- matrix (1, nrow=n)
Amat <- cbind(Amat, -diag(n))
bvec <- 1
meq <- 1
max.allocation <- 0.40
bvec <- c(bvec, rep(-max.allocation, n))

risk.premium.up <-0.3
risk.increment <- 0.01
loops <- risk.premium.up / risk.increment + 1

eff <- matrix(nrow = loops, ncol = n + 3)
colnames(eff) <- c(colnames(yield), "Std.Dev", "Exp.Return", "Sharpe")

#optimal
loop <- 1
mu <- colMeans(yield)
for (i in seq(from=0, to=risk.premium.up, by=risk.increment)){
  dvec <- mu * i 
  sol <- solve.QP(cov, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
  eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution *colSums((cov * sol$solution))))
  eff[loop,"Exp.Return"] <- as.numeric(sol$solution %*% colMeans(yield))
  eff[loop,"Sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
  eff[loop,1:n] <- sol$solution
  loop <- loop+1
}
eff <- as.data.frame(eff)
head(eff)
#optimal point
eff.optimal.point <- eff[eff$Sharpe == max(eff$Sharpe),]
{
  plot(eff$Std.Dev, eff$Exp.Return, type = 'b', 
       pch = 21, col = 'green', lwd = 2)
  points(eff.optimal.point$Std.Dev, eff.optimal.point$Exp.Return,
         pch = 20, col = 'red', lwd = 4)
  text(eff.optimal.point$Std.Dev, eff.optimal.point$Exp.Return-0.0005, 
       paste('Sharpe', round(eff.optimal.point$Sharpe, 3)))
  text(eff.optimal.point$Std.Dev, eff.optimal.point$Exp.Return-0.0008, 
       paste('Std.Dev', round(eff.optimal.point$Std.Dev, 3)))
  text(eff.optimal.point$Std.Dev, eff.optimal.point$Exp.Return-0.0011, 
       paste('Return', round(eff.optimal.point$Exp.Return, 3)))
}

eff.optimal.point
optimal.day.returns <- c()
for (i in 1 : nrow(yield)) {
  optimal.day.returns <- c(optimal.day.returns, 
                           as.numeric(yield[i,]) %*% as.numeric(eff.optimal.point[1 : 7]))
}

optimal.day.returns <- xts(optimal.day.returns, order.by = as.Date(row.names(yield)))
colnames(optimal.day.returns) <- "Returns"
(optimal.day.returns)

dim(Amat)
length(bvec)
Amat
bvec
d_tr <- as.matrix(eff.optimal.point[1 : n], nrow = 1)
d_tr
for (i in 1 : ncol(Amat)) {
  print(d_tr %*% Amat[, i])
}

# VAR
m <- mean(optimal.day.returns)
vol <- sd(optimal.day.returns)
alpha <- 0.95
money <- 1000000
(optimal.day.returns)
(VaR(optimal.day.returns, p = alpha, method = "historical") * money)
(var1.norm <- qnorm(1 - alpha, mean = m, sd = 1) * vol * money)
{
  hist(optimal.day.returns, col = "green", nclass = 20)
  abline(v = var1.norm, lwd = 2)
  text(var1.norm, 5, paste("var1 = ", var1.norm))
}
# expected shortfall
# normal
(ES(optimal.day.returns, p = alpha, method = "gaussian") * money)

# historical
(ES(optimal.day.returns, p = alpha, method = "historical") * money)
