# Melhor pacote para manipula��o de dados (pq carrega junto o dplyr)
library(tidyverse)

# Carregando os dados do exemplo
load("C:/Users/Klebson/Google Drive/Ensino/ECO905/ed_data.RData")
glimpse(ed_data)

# Nessa base de dados Trt_rand � um indicador de participa��o no programa de forma aleat�ria
# Podemos ver que 167 estudantes est�o no grupo de tratamento com o c�digo abaixo:

ed_data %>% 
  count(Trt_rand)

# devido a esta atribui��o de tratamento ter sido aleat�ria, n�o esperamos que o indicador de 
# tratamento de um aluno seja correlacionado com qualquer uma de suas outras caracter�sticas 
# pr�-tratamento

ed_data %>% 
  group_by(Trt_rand) %>% #constroi um tibble agrupada nos valores da variavel indicada
  summarise_if(is.numeric, mean) %>%  #para as variaveis numericas calcula a media por grupo
  select(-c(ID, Trt_non_rand)) #eliminando alguns variaveis da hora de mostrar o resultado

#Comparando com as medias dos grupo quando a atribui��o do programa n�o � aleat�ria
# veremos grande diferen�a nas medias de algumas variaveis entre os grupos

ed_data %>% 
  group_by(Trt_non_rand) %>% 
  summarise_if(is.numeric, mean) %>% 
  select(-c(ID, Trt_rand))

hist(ed_data$SES_CONT)

#Imagine que se proponha um novo curr�culo escolar que aumenta as notas dos alunos em matem�tica em 10 pontos, em m�dia. 
# Podemos usar R para imitar este processo e gerar aleatoriamente pontua��es p�s-teste que aumentam as pontua��es matem�ticas 
# do grupo de tratamento em 10 pontos em m�dia

ed_data <- ed_data %>% 
  mutate(MATH_post_trt_rand = 
           case_when(Trt_rand == 1 ~ MATH_pre + rnorm(1, 10, 2),
                     Trt_rand == 0 ~ MATH_pre + rnorm(1, 0, 2)),
         MATH_post_trt_non_rand = 
           case_when(Trt_non_rand == 1 ~ MATH_pre + rnorm(1, 10, 2), 
                     Trt_non_rand == 0 ~ MATH_pre + rnorm(1, 0, 2)))

# Olhando como ficam os resultados

ed_data %>% 
  select(ID, MATH_pre, Trt_rand, 
         MATH_post_trt_rand, Trt_non_rand, 
         MATH_post_trt_non_rand) %>% 
  filter(ID <= 10)


# Vamos examinar os efeitos nos grupos
ed_data %>% 
  group_by(Trt_rand) %>% 
  summarise(post_trt_rand_avg = mean(MATH_post_trt_rand))

# Fazendo a regress�o
fit <- lm(MATH_post_trt_rand ~ Trt_rand, data = ed_data)
summary(fit)



# Olhando para a estima��o em um ambiente sem randomiza��o
ed_data %>% 
  group_by(Trt_non_rand) %>% 
  summarise(post_trt_non_rand_avg = mean(MATH_post_trt_non_rand))

fit1_non_rand <- lm(MATH_post_trt_non_rand ~ Trt_non_rand, data = ed_data)
summary(fit1_non_rand)$coefficients


# O que � poss�vel afirmar sobre a aleatoriza��o?