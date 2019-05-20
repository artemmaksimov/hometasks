path <- './task_0'
data <- read.csv('./Documents/prak/notebooks/task_0/var16_Task0.csv')
library('MASS')
library(rriskDistributions)
head(data)
x <- data$x
shuffled_x = sample(x)
(length(shuffled_x))
first_half = shuffled_x[1:150]
second_half = shuffled_x[150:300]
hist(x ,col = 'green',lwd = 1, probability=1, breaks = 30)
fit_exp <- fitdistr(first_half, 'exponential')

(lambda <- 1 / (sum(first_half) / 150))
(ks.test(second_half,'pexp', lambda))
(ks.test(second_half,'pexp', fit_exp$estimate[1]))

fit_rchs$estimate[1]
fit.cont(first_half)
install.packages('Tktable')
install.packages("rriskDistributions")
library("rriskDistributions")
install.packages('XQuartz')
