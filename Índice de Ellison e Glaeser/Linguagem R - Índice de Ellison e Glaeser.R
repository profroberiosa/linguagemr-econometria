# �LVARO ROB�RIO - ECONOMIA REGIONAL - �NDICE DE ELLISSON E GLAESER (IEG)

# 1. PACOTE REAT (FERRAMENTAS DE AN�LISE REGIONAL)

# 1.1 INSTALA��O/ATIVA��O DO REAT 

install.packages("REAT")
library(REAT)

# 1.2 ESTAT�STICA DESCRITIVA DOS DADOS

summary(Base)
summary(Base$EMPREGO)

# 1.2 �NDICE IEG 

IEG <- ellison.a2 (Base$EMPREGO, Base$CNAE20, Base$MUNICIPIO)
IEGF <- data.frame(IEG)

# 1.3 EXPORTANDO RESULTADOS 

write.csv(IEGF, file="IEG2015.csv")
getwd()

# 1.4 HISTOGRAMA 

hist(OLS$Gamma.i,
     col = "lightgrey",
     xlab = "�ndice de Ellison e Glaeser",
     ylab = "N�mero de Ind�strias",
     widght= 2.0,
     )

# 1.5 OLS (TESTE DE ALEATORIEDADE)
install.packages("lmtest")
install.packages("car")
library(zoo)
library(car)
library(lmtest)

OLS1 <- lm(OLS$G.i ~ OLS$HHI.i)
coeftest(OLS1)
summary(OLS1)

# Teste Conjunto 
linearHypothesis(OLS1, c("(Intercept) = 0", "OLS$HHI.i= 1"))

# 1.6 FUN��O DE AGREGA��O DO EMPREGO

EINDUSTRIAL <- aggregate(Base$EMPREGO, by = list(Base$CNAE20), FUN = sum)
EMUNICIPAL <- aggregate(Base$EMPREGO, by = list(Base$MUNICIPIO), FUN = sum)

write.csv(EINDUSTRIAL, file="EINDUSTRIAL.csv")
getwd()

write.csv(EMUNICIPAL, file="EMUNICIPAL.csv")
getwd()

