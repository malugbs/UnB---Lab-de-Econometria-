library(haven)
library(tidyverse)
library(stargazer)

data <- read_dta("C:/Users/guima/Documents/R/Laboratório de Econometria/base pnad PBF.dta")

#Dividir os dados em dois grupos

grupo_bolsa <- data[data$bolsa_familia == 1, ]
grupo_nao_bolsa <- data[data$bolsa_familia == 0, ]

#Variáveis de interesse

variaveis <- c("cor", "rendimento", "idade", "meses_sem_emprego", "escolaridade")

#Inicializar vetores para armazenar médias e p-valores

medias_bolsa <- as.numeric()
medias_nao_bolsa <-  as.numeric()
p_valores <-  as.numeric()

#Looping através das variáveis de interesse para as médias e p-valor

for (i in 1:length(variaveis)) {
  # Calcular médias para o grupo Bolsa Família e o grupo Não Bolsa Família
  medias_bolsa[i] <- mean(grupo_bolsa[[variaveis[i]]])
  medias_nao_bolsa[i] <- mean(grupo_nao_bolsa[[variaveis[i]]])
  
  # Realizar um teste t para a diferença de médias
  resultado_teste <- t.test(grupo_bolsa[[variaveis[i]]], grupo_nao_bolsa[[variaveis[i]]])
  
  # Armazenar o p-valor
  p_valores[i] <- resultado_teste$p.value
}

# Criar um data frame com as médias e p-valores

resultados <- data.frame(Variavel = variaveis, Media_Bolsa = medias_bolsa, Media_Nao_Bolsa = medias_nao_bolsa, P_Valor = p_valores)
print(resultados)

# Definir a semente
set.seed(211014780)

# Randomizar a variável bolsa_familia
data$bolsa_familia_rand <- ifelse(runif(nrow(data)) > 0.5, 1, 0)

# Dividir o banco de dados em dois grupos com base na coluna 'bolsa_familia_rand'.
grupo_bolsa_rand <- data[data$bolsa_familia_rand == 1, ]
grupo_nao_bolsa_rand <- data[data$bolsa_familia_rand == 0, ]

# Variáveis de interesse
variaveis <- c("cor", "rendimento", "idade", "meses_sem_emprego", "escolaridade")

# Inicializar vetores para armazenar médias e p-valores
medias_bolsa_rand <- as.numeric()
medias_nao_bolsa_rand <- as.numeric()
p_valores_rand <- as.numeric()

# Loop através das variáveis de interesse
for (i in 1:length(variaveis)) {
  # Calcular médias para o grupo Bolsa Família randomizado e o grupo Não Bolsa Família randomizado
  medias_bolsa_rand[i] <- mean(grupo_bolsa_rand[[variaveis[i]]])
  medias_nao_bolsa_rand[i] <- mean(grupo_nao_bolsa_rand[[variaveis[i]]])
  
  # Realizar um teste t para a diferença de médias
  resultado_teste <- t.test(grupo_bolsa_rand[[variaveis[i]]], grupo_nao_bolsa_rand[[variaveis[i]]])
  
  # Armazenar o p-valor
  p_valores_rand[i] <- resultado_teste$p.value
}

# Criar um data frame com as médias e p-valores
resultados_rand <- data.frame(Variavel = variaveis, Media_Bolsa_Randomizado = medias_bolsa_rand, Media_Nao_Bolsa_Randomizado = medias_nao_bolsa_rand, P_Valor_Randomizado = p_valores_rand)

# Exibir os resultados
print(resultados_rand)


#Criando um data frame com todos os resultados
resultados_comp <- data.frame(Variavel = variaveis, Bolsa = medias_bolsa,  Nao_Bolsa = medias_nao_bolsa, Bolsa_Randomizado = medias_bolsa_rand, Nao_Bolsa_Randomizado = medias_nao_bolsa_rand, p_valor = p_valores, p_valor_Random = p_valores_rand)
  
print(resultados_comp)


#Gráficos
mytheme <- 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top")

data %>%
  mutate(bolsa = ifelse(bolsa_familia == 1, "Com bolsa família", "Sem bolsa família")) %>%
  ggplot(aes(x = bolsa, y = rendimento, fill = bolsa)) +
  geom_boxplot() +
  scale_fill_manual(values = met.brewer('Tara')) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  labs(x = '',
       y = '',
       title = 'Diferença de rendimentos entre grupos',
       subtitle = 'Sem randomização',
       fill = '') +
  mytheme

data %>%
  mutate(bolsa = ifelse(bolsa_familia == 1, "Com bolsa família", "Sem bolsa família")) %>%
  ggplot(aes(x = escolaridade, fill = bolsa)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_fill_manual(values = met.brewer('Tara')) +
  labs(x = '',
       y = '',
       title = 'Diferença de escolaridade entre grupos',
       subtitle = 'Sem randomização',
       fill = '') +
  mytheme


data_rand <- rbind(grupo_bolsa_rand, grupo_nao_bolsa_rand)

data_rand %>%
  mutate(bolsa = ifelse(bolsa_familia_rand == 1, "Com bolsa família", "Sem bolsa família")) %>%
  ggplot(aes(x = bolsa, y = rendimento, fill = bolsa)) +
  geom_boxplot() +
  scale_fill_manual(values = met.brewer('Tara')) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  labs(x = '',
       y = '',
       title = 'Diferença de rendimentos entre grupos',
       subtitle = 'Com randomização',
       fill = '') +
  mytheme

data %>%
  mutate(bolsa = ifelse(bolsa_familia_rand == 1, "Com bolsa família", "Sem bolsa família")) %>%
  ggplot(aes(x = escolaridade, fill = bolsa)) +
  geom_density(adjust=1.5, alpha=.4) +
  scale_fill_manual(values = met.brewer('Tara')) +
  labs(x = '',
       y = '',
       title = 'Diferença de escolaridade entre grupos',
       subtitle = 'Com randomização',
       fill = '') +
  mytheme

