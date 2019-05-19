library('gsl')
library('copula')

data <- readRDS("var_16_Task_5.rds")
names(data)

plot(data$predictor, data$output, main = 'predictor vs output', col = 'green',
     pch = 20, xlab = 'predictor', ylab = 'output')

# empirical copula
e_cop <- pobs(cbind(data$predictor, data$output))
plot(e_cop[, 1], e_cop[, 2], pch = 21, main ="pobs(predictor vs output) ", col = "blue")


#Normal copula
normal_copula <- normalCopula(param = 0, dim = 2)
#Student copula
t_copula <- ellipCopula(family = "t", param = 0, dim = 2)
#Frank copula
Frank_copula <- frankCopula(param = 5, dim = 2)
#Clayton
Clayton_copula <- claytonCopula(param = 5, dim = 2)

myFitCopula <- function(type) {
  if (type == "Clayton")
    tryCatch( 
      {
        y = fitCopula(Clayton_copula,
                      e_cop,
                      method = "ml",
                      optim.method = "BFGS", 
                      optim.control = list(maxit=1000))
        return(y)
      },
      error=function(error_message) {
        return(NA)
      }
    )
  if (type == "Normal") {
    Gaussian.Copula.fit<-fitCopula(normal_copula, 
                                   e_cop, 
                                   method = "ml",
                                   optim.method = "BFGS", 
                                   optim.control = list(maxit=1000))
    parameters <- Gaussian.Copula.fit
    return (parameters)
  }
  
  if (type == "Student") {
    t.Copula.fit<-fitCopula(t_copula, 
                            e_cop, 
                            method = "ml",
                            optim.method = "BFGS", 
                            optim.control = list(maxit=10000))
    parameters <- t.Copula.fit
    return (parameters)
  }
  
  if (type == "Frank") {
    Frank.Copula.fit<-fitCopula(Frank_copula, 
                                e_cop, 
                                method = "ml",
                                optim.method = "BFGS", 
                                optim.control = list(maxit=1000))
    parameters <- Frank.Copula.fit
    return (parameters)
  }
}

copula.c <- myFitCopula("Clayton")
(copula.c@loglik)

copula.n <- myFitCopula("Normal")
(copula.n@loglik)

copula.t <- myFitCopula("Student")
(copula.t@loglik)

copula.f <- myFitCopula("Frank")
(copula.f@loglik)

# max loglik - Frank
best_parameters <- copula.f@copula@parameters

# plot
frankCopula.Object <- frankCopula(param = best_parameters, dim = 2)
persp(frankCopula.Object, dCopula, main = "pdf",
      xlab = "u", ylab = "v", zlab = "c(u,v)")

contour(claytonCopula.Object, dCopula, 
        main = "pdf", xlab = "u", ylab = "v")

data$predictor_DistrType
data$predictor_DistrParameters

data$output_DistrType
data$output_DistrParameters

predictor.copula <- plogis(data$predictor, data$predictor_DistrParameters[1], data$predictor_DistrParameters[2])
output.copula <- pexp(data$output, data$output_DistrParameters[1], data$output_DistrParameters[2])

plot(predictor.copula, output.copula, 
     main = 'predictor vs output. Marginal Distribution Copula', 
     col = 'green', pch = 20, xlab = 'predictor', ylab = 'output')

copula <- cbind(predictor.copula, output.copula)
alpha <- 0.95
theta <- best_parameters
theta
quantile <- (- 1 / theta) * log(1 - alpha * (1 - exp(-theta)) / (exp(-theta*copula[, 1]) + alpha * (1 - exp(-theta*copula[, 1]))))


(anomalindex <- which(copula[, 2] > quantile))

plot(copula[, 1], copula[, 2], pch = 20, col = "blue", main = "quatile level 95%")
points(copula[, 1], quantile, col = "green", pch = 20)
points(copula[anomalindex, 1], copula[anomalindex, 2], col = "magenta", pch = 20)

anomal_predictor <- data$predictor[anomalindex]
anomal_output    <- data$output[anomalindex]
head(anomal_predictor)
head(anomal_output)

plot(data$predictor, data$output, pch = 21,
     col = "blue", main = "predictor vs output anomalies")
points(anomal_predictor, anomal_output, pch = 21, col = "magenta")

variant <- 16
copulaName <- "frank"

myResult <- list(variant = variant,
                 copulaName = copulaName,
                 predictor.copula = predictor.copula,
                 output.copula = output.copula,  
                 best_parameters = best_parameters,
                 anomal_predictor= anomal_predictor,
                 anomal_output= anomal_output)

myResult$copulaName
myResult$best_parameters

