# Aluno: Álvaro Robério - Mestrado em Economia -  Atividade Prática de Econometria 1 

# Geração das Variáveis 

N <- nrow(LNTC)
LNTC <- as.matrix(log (Base$TC))
LNQ <- as.matrix(log (Base$Q))
LNPF <- as.matrix(log (Base$PF))
LNPL <- as.matrix(log (Base$PL))
LNPK <- as.matrix(log (Base$PK))

LNTCPF <- as.matrix(LNTC-LNPF) 
LNPLPF <- as.matrix(LNPL - LNPF)
LNPKPF <- as.matrix(LNPK - LNPF)

# Resolução da Questão (B)

#  Modelo Irrestrito 

OLSIR <- lm(LNTC ~ LNQ + LNPL + LNPF + LNPK)
summary(OLSIR)

SSRIR <-sum(resid(OLSIR)^2)
print(SSRIR)

# Plotagem de Gráfico - Modelo Irrestrito

plot(LNQ, OLSRE$residuals, xlab="Ln(produção)", ylab = "Resíduos")
abline(a=0, b=0, lwd=2)

plot(OLSRE$fitted.values, OLSRE$residuals, xlab="Saída", ylab = "Resíduos")
abline(a=0, b=0, lwd=2)

# Resolução da Questão (C)

# Modelo Restrito 

OLSRE <- lm(LNTCPF ~ LNQ + LNPLPF + LNPKPF)
summary(OLSRE)
SSRRE <- sum(resid(OLSRE)^2)
print(SSRRE)

# Teste F 

NUM <- (SSRRE-SSRIR)/1
DEN <- SSRIR/(N-5)
FTEST <- NUM/DEN
print(FTEST)

# Resolução da Questão (D)

# Modelo 1 - Coeficiente e Variança Diferentes (Grupos)

# Grupo A - MQO, Beta e Estimação do Erro 

OLSRE1 <- lm(LNTCPF[1:29,]~ LNQ[1:29,] + LNPLPF[1:29,] + LNPKPF[1:29,])
summary(OLSRE1)
SSRRE1 <-sum(resid(OLSRE1)^2)
print(SSRRE1)

X1 <- cbind(1, LNQ[1:29,], LNPLPF[1:29,], LNPKPF[1:29,] )
Y1 <- cbind(LNTCPF[1:29,])
GB1 <- solve(t(X1)%*%X1)%*%t(X1)%*%Y1 
head(GB1)

E1 <-  Y1 - X1%*%GB1
S2E1 <- as.numeric(t(E1)%*%E1/(29-4))
head(S2E1)

# Grupo B - MQO, Beta e Estimação do Erro 
                         
OLSRE2 <- lm(LNTCPF[30:58,]~ LNQ[30:58,] + LNPLPF[30:58,] + LNPKPF[30:58,])
summary(OLSRE2)
SSRRE2 <-sum(resid(OLSRE2)^2)
print(SSRRE2)

X2 <- cbind(1, LNQ[30:58,], LNPLPF[30:58,], LNPKPF[30:58,] )
Y2 <- cbind(LNTCPF[30:58,])
GB2 <- solve(t(X2)%*%X2)%*%t(X2)%*%Y2 
head(GB2)

E2 <-  Y2 - X2%*%GB2
S2E2 <- as.numeric(t(E2)%*%E2/(29-4))
head(S2E2)

# Grupo C - MQO, Beta e Estimação do Erro 

OLSRE3 <- lm(LNTCPF[59:87,]~ LNQ[59:87,] + LNPLPF[59:87,] + LNPKPF[59:87,])
summary(OLSRE3)
SSRRE3 <-sum(resid(OLSRE3)^2)
print(SSRRE3)

X3 <- cbind(1, LNQ[59:87,], LNPLPF[59:87,], LNPKPF[59:87,])
Y3 <- cbind(LNTCPF[59:87,])
GB3 <- solve(t(X3)%*%X3)%*%t(X3)%*%Y3 
head(GB3)

E3 <-  Y3 - X3%*%GB3
S2E3 <- as.numeric(t(E3)%*%E3/(29-4))
head(S2E3)

# Grupo D - MQO, Beta e Estimação do Erro 

OLSRE4 <- lm(LNTCPF[88:116,]~ LNQ[88:116,] + LNPLPF[88:116,] + LNPKPF[88:116,])
summary(OLSRE4)
SSRRE4 <-sum(resid(OLSRE4)^2)
print(SSRRE4)

X4 <- cbind(1, LNQ[88:116,], LNPLPF[88:116,], LNPKPF[88:116,])
Y4 <- cbind(LNTCPF[88:116,])
GB4 <- solve(t(X4)%*%X4)%*%t(X4)%*%Y4 
head(GB4)

E4 <-  Y4 - X4%*%GB4
S2E4 <- as.numeric(t(E4)%*%E4/(29-4))
head(S2E4)

# Grupo E - MQO, Beta e Estimação do Erro 

OLSRE5 <- lm(LNTCPF[117:145, ]~ LNQ[117:145, ]+ LNPLPF[117:145,] + LNPKPF[117:145,])
summary(OLSRE5)
SSRRE5 <-sum(resid(OLSRE5)^2)
print(SSRRE5)

X5 <- cbind(1, LNQ[117:145, ], LNPLPF[117:145, ], LNPKPF[117:145, ])
Y5 <- cbind(LNTCPF[117:145, ])
GB5 <- solve(t(X5)%*%X5)%*%t(X5)%*%Y5 
head(GB5)

E5 <-  Y5 - X5%*%GB5
S2E5 <- as.numeric(t(E5)%*%E5/(29-4))
head(S2E5)

# Resolução da Questão (E)

# Modelo 2 - Coeficiente e Variança Diferentes (Grupos)

# Geração de Variáveis 

d1 <- as.numeric(Base$grupo == 1)
d2 <- as.numeric(Base$grupo == 2)
d3 <- as.numeric(Base$grupo == 3)
d4 <- as.numeric(Base$grupo == 4)
d5 <- as.numeric(Base$grupo == 5)

LNQ1 <- d1*LNQ
LNQ2 <- d2*LNQ
LNQ3 <- d3*LNQ
LNQ4 <- d4*LNQ
LNQ5 <- d5*LNQ

LNPLPF1 <- d1*LNPLPF
LNPLPF2 <- d2*LNPLPF
LNPLPF3 <- d3*LNPLPF
LNPLPF4 <- d4*LNPLPF
LNPLPF5 <- d5*LNPLPF

LNPKPF1 <- d1*LNPKPF
LNPKPF2 <- d2*LNPKPF
LNPKPF3 <- d3*LNPKPF
LNPKPF4 <- d4*LNPKPF
LNPKPF5 <- d5*LNPKPF

# Modelo 2 Geral 

OLSIRG <- lm(LNTCPF  ~ LNQ1 + LNQ2 + LNQ3 + LNQ4 + LNQ5 + LNPLPF1 + LNPLPF2 + LNPLPF3 + LNPLPF4 + LNPLPF5 + LNPKPF1 + LNPKPF2 + LNPKPF3 + LNPKPF4 + LNPKPF5 + d2 + d3 + d4 + d5)
summary(OLSIRG)
SSRIRG <-sum(resid(OLSIRG)^2)
print(SSRIRG)

# Resolução da Questão (F)

# Testando a H0: Bn=0 e H1: Bn !=0 (Teste de Chow) - 5.97473*

# q=(GLR-GLIR)=141-125=16 
# O GLR e GLIR (Graus de Liberdade) são dados na equação de OLSRE e OLSIRG.

summary(OLSRE)
summary(OLSIRG)

NUMERADOR <- (21.64032-12.26243)/16
DENOMINADOR <- (12.26243)/(145-20)
TCHOW <- (NUMERADOR/DENOMINADOR)
print(TCHOW)
