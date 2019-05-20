tr_rates <- read.csv('Data.csv', sep=';')
library(MASS)
head(tr_rates)
(ndays  <-  dim(tr_rates)[1])
(ncol  <-  dim(tr_rates)[2])

library(xts)
tr_dates <- as.Date(tr_rates[,1],"%m/%d/%y")
rates.xts <- as.xts(tr_rates[2:ncol], order.by = tr_dates)
head(rates.xts)
plot(rates.xts,type = 'b',pch =20,main = "US Treasury Rates")

pca <- prcomp(rates.xts, scale = FALSE)
summary(pca)


ss <- summary(pca)
his <- ss$importance[3,]
l <- length(his)
his[4:l]<- NA
r <- as.matrix(cbind(ss$importance[3,],his))

mp <- matplot(1:l,r,type = 'h',lty = 1, lwd = 10,main = "Cumulative proportion.", col = c("blue",'red')) # default
abline(h = 0.8,col = 'black')

tot <- ss$importance[2,]
rownames(ss$rotation)
matplot(cbind(ss$rotation[,1],ss$rotation[,2],ss$rotation[,3]),type = 'b',pch=21,lwd = 2,
        col = c("blue","green","magenta"),main= "Shift,Twist,Butterfly",ylab = "loadings",xlab="maturity",lty=1 )
legend("bottomleft",c("Shift","Twist","Butterfly"),lty=c(1,1,1),lwd = 2,col = c("blue","green","magenta"))


scores <- function(ldata,pca,number)
{
  cdata <- ldata
  m <- dim(ldata)[2]
  for (i in 1:m)
    cdata[,i] <- ldata[,i] - pca$center[i]
  loads <- pca$rotation[,1:number]
  cdata <- as.matrix(cdata)
  f <- cdata %*% loads
  return (f)
}



number <- 3
fscores <- scores(rates.xts,ss,number)
matplot(fscores,type = 'l',lty = 1, col = 1:number,main = 'Factor Scores')
legend('bottomright',c('shift','twist','butterfly'),col = 1:number,lty = 1)
abline(h=0)

fscores_diff<-diff(fscores[,1:3])
matplot(fscores_diff,type = 'l',lty = 1, col = 1:number,main = 'Factor Scores Diff')
legend('topright',c('shift','twist','butterfly'),col = 1:number,lty = 1)
cor(fscores_diff)

m<-c(mean(fscores_diff[,1]), mean(fscores_diff[,2]),mean(fscores_diff[,3]))

arr<-mvrnorm(n = 1000, mu = m, Sigma = cov(fscores_diff))
shift1000<-arr[,1]
twist1000<-arr[,2]
but1000 <- arr[,3]




shift_new<-shift1000 + fscores[nrow(fscores),1]
twist_new<-twist1000 + fscores[nrow(fscores),2]
but_new<-but1000 + fscores[nrow(fscores),3]

restoreData<- function(fscores,loadings,center)
{
  npca <- dim(fscores)[2]
  myeigen <- t(loadings[,1:npca])
  rest <- fscores %*%myeigen
  m <- length(center)
  if (m == dim(rest)[2])
  {
    for (i in 1:m)
      rest[,i] <- rest[,i]+center[i]
  }
  return(rest)
}
rest <- restoreData(cbind(shift_new, twist_new, but_new),pca$rotation,pca$center)
head(rest)

beta <- c(10,15,10,15,20,20,10,0,0,10,15)

tau <- c(1/12,0.25,0.5,1,2,3,5,7,10,20,30)
par<-c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000)
m = c(2,2,2,2,2,2,2,2,2,2,2)
coupon <- c(0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03)

library(FinAna)
price <- c()
for (i in 1:1000)
{
  yield <- unname(rest[i,])
  cost <- 0
  for (j in 1:11) {
    cost <-cost + beta[j] * bond.price(par[j],coupon[j],tau[j],yield[j],m[j])
  }
  price <- append(price, cost)
}

tot <- ss$importance[2,]
rownames(ss$rotation)
matplot(cbind(ss$rotation[,1],ss$rotation[,2],ss$rotation[,3]),type = 'b',pch=21,lwd = 2,
        col = c("blue","green","magenta"),main= "Shift,Twist,Butterfly",ylab = "loadings",xlab="maturity",lty=1 )
legend("bottomleft",c("Shift","Twist","Butterfly"),lty=c(1,1,1),lwd = 2,col = c("blue","green","magenta"))


scores <- function(ldata,pca,number)
{
  cdata <- ldata
  m <- dim(ldata)[2]
  for (i in 1:m)
    cdata[,i] <- ldata[,i] - pca$center[i]
  loads <- pca$rotation[,1:number]
  cdata <- as.matrix(cdata)
  f <- cdata %*% loads
  return (f)
}



number <- 3
fscores <- scores(rates.xts,ss,number)
matplot(fscores,type = 'l',lty = 1, col = 1:number,main = 'Factor Scores')
legend('bottomright',c('shift','twist','butterfly'),col = 1:number,lty = 1)
abline(h=0)

fscores_diff<-diff(fscores[,1:3])
matplot(fscores_diff,type = 'l',lty = 1, col = 1:number,main = 'Factor Scores Diff')
legend('topright',c('shift','twist','butterfly'),col = 1:number,lty = 1)
cor(fscores_diff)

m<-c(mean(fscores_diff[,1]), mean(fscores_diff[,2]),mean(fscores_diff[,3]))

arr<-mvrnorm(n = 1000, mu = m, Sigma = cov(fscores_diff))
shift1000<-arr[,1]
twist1000<-arr[,2]
but1000 <- arr[,3]




shift_new<-shift1000 + fscores[nrow(fscores),1]
twist_new<-twist1000 + fscores[nrow(fscores),2]
but_new<-but1000 + fscores[nrow(fscores),3]

restoreData<- function(fscores,loadings,center)
{
  npca <- dim(fscores)[2]
  myeigen <- t(loadings[,1:npca])
  rest <- fscores %*%myeigen
  m <- length(center)
  if (m == dim(rest)[2])
  {
    for (i in 1:m)
      rest[,i] <- rest[,i]+center[i]
  }
  return(rest)
}
rest <- restoreData(cbind(shift_new, twist_new, but_new),pca$rotation,pca$center)
head(rest)

beta <- c(10,15,10,15,20,20,10,0,0,10,15)

tau <- c(1/12,0.25,0.5,1,2,3,5,7,10,20,30)
par<-c(1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,1000)
m = c(2,2,2,2,2,2,2,2,2,2,2)
coupon <- c(0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03,0.03)

library(FinAna)
price <- c()
for (i in 1:1000)
{
  yield <- unname(rest[i,])
  cost <- 0
  for (j in 1:11) {
    cost <-cost + beta[j] * bond.price(par[j],coupon[j],tau[j],yield[j],m[j])
  }
  price <- append(price, cost)
}

price_lastday<-0
for (j in 1:11) {
  price_lastday <-price_lastday + beta[j] * bond.price(par[j],coupon[j],tau[j],rates.xts[nrow(rates.xts),j],m[j])
}


dif <- price - rep(price_lastday, times = 1000)
mean_dif <- mean(dif)


hist(dif,col = "green",nclass = 20)
# normal
(es_norm = -170.752028754919)

# historical
(es_hist =-173.924361081995)
abline(v = es_hist,lwd = 2)
abline(v = es_norm,lwd = 2)

text(es_hist,5,paste("ES_hist",es_hist))
text(es_norm,5,paste("ES_norm",es_norm))

library(PerformanceAnalytics)
# normal
(ES(dif, p = alpha, method = "gaussian"))

# historical
(ES(dif, p = alpha, method = "historical"))

