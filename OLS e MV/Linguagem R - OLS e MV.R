# Exemplo Teórico

# Gerando um exemplo teórico
n <- 100
k <- 2
x1 <- rnorm(n,7,2)
y1 <- rnorm(length(x1), 2*x1 + 1, 1)

plot(x1,y1)

# Estimando via MLE
install.packages("bbmle")
library(bbmle)

funcaomv = function(b0, b1, sigma){
  Y.pred = b0 + b1*x1
  -sum(dnorm(y1, mean = Y.pred, sd = sigma, log = TRUE ))
}

(reg.mle <- mle2(funcaomv, start = list(b0=14, b1=0, sigma=1)))


# Intervalo de Confiança
confint(reg.mle)

profile.reg.mle <- profile(reg.mle)
par(mfrow=c(1,3))
plot(profile.reg.mle, abs=T, conf = c(99, 95, 90, 80, 50)/100)

vcov(reg.mle)
vcov(reg.ols)


rm(list = ls())

# Estimação no Formato Matricial

library(foreign)
gpa1 <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/gpa1.dta")

#determinando o tamanho da amostra e do numero de parametros
n <- nrow(gpa1); k <- 3

y <- gpa1$colGPA
X <- cbind(1, gpa1$hsGPA, gpa1$ACT)

head(X)

bhat <- solve(t(X)%*%X)%*%t(X)%*%y

#Residuos, estimacao da variancia dos erros

ehat <- y - X%*%bhat

# estimativa de sigma2 via OLS
sigma2.hat <- as.numeric(t(ehat)%*%ehat/(n-k))

# variancia-covariancia dos betas
vcov.ols <- sigma2.hat*solve(t(X)%*%X)


data_matrix <- X


library(bbmle)

funcaomv = function(b0, b1, b2, sigma, x=data_matrix){
  Y.pred = b0 + b1*x[,2] + b2*x[,3]
  -sum(dnorm(gpa1$colGPA, mean = Y.pred, sd = sigma, log = TRUE ))
}

#Valores Iniciais
start.b0 <- mean(y)
start.b1 <- mean(X[,2])
start.b2 <- mean(X[,3])
start.sigma <- sd(y)/sqrt(length(y))

(reg.mle <- mle2(funcaomv, start = list(b0=start.b0, b1=start.b1, b2=start.b2, sigma=start.sigma), method = "BFGS"))

vcov(reg.mle)

#compare com os resultados de OLS


# Tira notação científica

options(scipen = 999)



