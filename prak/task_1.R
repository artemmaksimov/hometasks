data <- read.csv('./Documents/prak/notebooks/task_1/var_16_Task_1.csv', header = T)
head(data)
{
  regr <-  lm(data$y ~ data$pr1 + data$pr2 + data$pr3 + data$pr4 + data$pr5, data=data)
  summary(regr)
}
{
  regr_fixed <-  lm(data$y ~ data$pr4 + data$pr5, data=data)
  summary(regr_fixed)
}
{
  plot(data$y, type='b', col='deepskyblue4', xlab='Measure', main='y')
  lines(regr_fixed$fitted.values, col='firebrick1', lwd=3)
}
{
  regr <-  lm(data$y ~ data$pr2, data=data)
  summary(regr)
}

res <- regr_fixed$residuals
plot(regr$residuals, type='b', col='blue4', xlab='Measure', main='Residuals')

hist(rstudent(regr), xlab='Standardized Residuals', col="blue", breaks=20)

acf(res)
{
  library('TSA')
  k <- kernel('daniell', c(5, 5, 5))
  #plot(k, type='h', lwd=4, col='blue')
  per <- periodogram(res, plot=FALSE)
  
  sp_raw <- spec(res, col='blue', lwd=2, xlab= 'Частота', ylab = 'Спектральная плотность')
  sp_smooth <- spec(res, kernel=k, log='no', sub='', xlab='Частота', ylab='Сглаженная спектральная плотность', col='blue', lwd=2)
  lines(sp_raw$freq, sp_raw$spec, lwd=2, col='red')
  lines(per$freq, per$spec, col="magenta")
  legend("topleft",c("Raw", "Daniell^3 window", "Periodogram"), bty="n", lwd=2, col=c("red", "blue", "magenta"))
}

install.packages('nortest')
library('nortest')
ks.test(rstudent(regr_fixed), "pnorm", 0, 1) # отвергаем
pearson.test(rstudent(regr_fixed)) # отвергаем
shapiro.test(rstudent(regr_fixed)) # отвергаем

install.packages('randtests')
library('randtests')
rank.test(res) # не отвергаем
bartels.rank.test(res) # не отвергаем
cox.stuart.test(res) # не отвергаем
runs.test(res) # не отвергаем
turning.point.test(res) # не отвергаем
# не отвергаем голосованием

# Проверяем независимость
# По АКФ не являются независимыми

# Определяем эховость по кепстру
{
  l <- Re(fft(log(abs(fft(res))), inverse = TRUE))
  l[1]=0
  barplot(l[0:50],col = "blue",main="Kepstr")
}
