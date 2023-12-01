library(haven)
library(tidyverse)
library(stargazer)
library(AER)
library(data.table)
require(aod)
library(skimr)
library(knitr)
library(kableExtra)
install.packages("ivprobit")

#Passo 1: Criar um banco de dados

pnad2005namepath <- "C:/Users/guima/Documents/R/Laboratório de Econometria/PES2015.txt"
df.pnad2005.colunas.nomes <- c('uf', 'V0302', 'V1101', 'V1141', 'V1142','V1151', 'V1152', 'V1182','V1109','V112', 'V1113', 'ocupaçao')

df.pnad2005.colunas.limites <-list(beg = c(5,18, 666, 667,669,671,673,686,690,692,694, 706),end = c(6,18,666,668,670,672,674,689,690,693,695, 706))
  
df.pnad2005 = fread(pnad2005namepath, header = FALSE, sep = "",na.strings = c("NA","N/A","", " "),skip = 0L, stringsAsFactors = FALSE, strip.white = TRUE
)[, lapply(1:(length(df.pnad2005.colunas.limites$beg)), 
           function(ii){ as.numeric(substr(V1, df.pnad2005.colunas.limites$beg[ii], df.pnad2005.colunas.limites$end[ii]))})]

colnames(df.pnad2005) <- df.pnad2005.colunas.nomes

#Variável Dependente (Y): Participação no mercado de trabalho (por exemplo, 1 para empregada, 0 para desempregada).
#Variável Endógena (X): Se a mulher tem exatamente um filho vivo (1 para sim, 0 para não).
#Variável Instrumental (Z): Se a mulher teve um ou mais filhos natimortos e não teve outros filhos (1 para sim, 0 para não).Passo 1: Criar um banco de dados
df.pnad2005[is.na(df.pnad2005)] <- 0

df.pnad2005 <- df.pnad2005 %>%
  filter(V0302 == 4) %>%
  filter(V1109 == 1) %>% #o último filho nascido vivo ainda está vivo
  filter(V1182 > 2008) %>% #O último filho vivo tem no máximo 6 anos
  mutate(filhos_vivos = ifelse(V1101 == 1, 1, 0), #teve filhos vivos
         n_filhos_vivos = V1141 + V1142 + V1151 + V1152, #filhos vivos, morando junto ou não
         filhos = ifelse(n_filhos_vivos == 1, 1, 
                         ifelse(n_filhos_vivos == 0, 0, 2)), #apenas um, dois ou mais filhos vivos
         n_filhos_natimortos = V112 + V1113, #quantos filhos natimortos
         nat = ifelse(n_filhos_natimortos > 0, 1, 0), 
         ocupaçao = ifelse(ocupaçao == 1, 1, 0)) #ocupada na semana de referencia

df.pnad2005 <- df.pnad2005 %>%
  select(ocupaçao, filhos, n_filhos_vivos, nat, n_filhos_natimortos,V1182)


#Passo 2: Realizar a regressão simples por OLS
reg <- lm(ocupaçao ~ n_filhos_vivos, df.pnad2005)
stargazer(reg, type = 'text')

reg_2 <- lm(ocupaçao ~ n_filhos_natimortos, df.pnad2005)
stargazer(reg, reg_2, type = 'text')

reg_3 <- lm(n_filhos_vivos ~ n_filhos_natimortos, df.pnad2005)
stargazer(reg_3, type = 'text')


#Passo 3: Realizar a regressão por Variáveis Instrumentais (IV)
#Realizar uma regressão por Variáveis Instrumentais (IV) usando a variável instrumental (Z) como um instrumento para a variável endógena (X). A equação de regressão IV seria:
fm <- ivreg(ocupaçao ~ n_filhos_vivos | n_filhos_natimortos,
            data = df.pnad2005)

stargazer(fm, reg, type = 'text')
summary(fm, vcov = sandwich, df = Inf, diagnostics = TRUE)

#Compare os resultados obtidos na regressão simples por OLS com os resultados da regressão por Variáveis Instrumentais. Observe se a inclusão da variável instrumental (Z) teve algum impacto nos coeficientes e na interpretação do efeito da variável endógena (X) na participação no mercado de trabalho (Y).








myprobit <- glm(ocupaçao ~ filho_unico , family = binomial(link = "probit"), 
                data = df.pnad2005)
stargazer(myprobit, type = 'text')

myprobit2 <- glm(ocupaçao ~ apenas_nat , family = binomial(link = "probit"), 
                data = df.pnad2005)
stargazer(myprobit, myprobit2, type = 'text')

