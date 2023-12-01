library(tidyverse) #Data tiding - contain ggplot2 and other
library(foreign)# read data of other types
library(plm) # Linear models for panel data
library(stargazer)# tables for TeX
library(haven) # better output tables - allows exporting table to html and pdf.tex formats
library(expss)
library("haven")

diff <- read_dta("C:/Users/guima/Documents/R/Laboratório de Econometria/DiD/banco_diff_diff.dta")

#Passo 1: Estime por OLS e interprete os coeficientes dos efeitos do tratamento sobre as proficiências de leitura e matemática. O programa foi efetivo? 
#proficiência_it = α + β1 · grupo_tratado_i · d.2007_t + β2 · grupo_tratado_i + β3 · d.2007_i + ε_i      (1);

ols_mat <- lm(profic_mat~grupo_tratado + ano_2007 + grupo_tratado*ano_2007, diff)
ols_port <- lm(profic_port~grupo_tratado + ano_2007 + grupo_tratado*ano_2007, diff)

stargazer(ols_mat, ols_port, type = 'text')

#Passo 2: Informe a inclinação dos grupos de escolas tratadas e de controle e seus interceptos.

pi_cont_mat <- ols_mat[["coefficients"]][["(Intercept)"]]
pi_treat_mat <- ols_mat[["coefficients"]][["grupo_tratado"]] + ols_mat[["coefficients"]][["(Intercept)"]]
pti_cont_mat <- ols_mat[["coefficients"]][["(Intercept)"]] + ols_mat[["coefficients"]][["ano_2007"]]
pti_treat_mat <- ols_mat[["coefficients"]][["(Intercept)"]] + ols_mat[["coefficients"]][["ano_2007"]] + ols_mat[["coefficients"]][["grupo_tratado:ano_2007"]] + ols_mat[["coefficients"]][["grupo_tratado"]] 

##Counterfactual=β0+β1+β2
counterfactual_mat <- ols_mat[["coefficients"]][["(Intercept)"]] + ols_mat[["coefficients"]][["ano_2007"]] + ols_mat[["coefficients"]][["grupo_tratado"]] 

const_diff = pi_treat_mat - pi_cont_mat
int_effect_mat <- pti_treat_mat - counterfactual_mat

time <- c('Before Intervention', 'Post-Intervention', 'Before Intervention', 'Post-Intervention', 'Before Intervention','Post-Intervention')
group <- c('Control', 'Control', 'Treatment', 'Treatment', 'Counterfactual','Counterfactual')
results <- c(pi_cont_mat, pti_cont_mat, pi_treat_mat, pti_treat_mat, pi_treat_mat,counterfactual_mat)

graph_mat <- data.frame(time, group, results)

p <- ggplot(graph_mat, aes(x = time, y = results, group = group, color = group)) +
  geom_line(size = 2) +
  geom_point() +
  geom_segment(aes(x = 'Post-Intervention', xend = 'Post-Intervention', y = counterfactual_mat, yend = pti_treat_mat),
               linetype = "dashed", color = "black", size = 1) +
  geom_segment(aes(x = 'Post-Intervention', xend = 'Post-Intervention', y = counterfactual_mat, yend = pti_cont_mat),
               linetype = "dotted", color = "black", size = 1)  +
  labs(title = "Gráfico de DiD",
       subtitle = 'Matemática',
       x = "Período",
       y = "",
       color = "") +
  theme_light()  +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top")

p + annotate("text", x = c(2.15), y = c(206), 
             label = c("Efeito do\n tratamento") , color="black", 
             size=4 , fontface="bold")


#para português

pi_cont_pt <- ols_port[["coefficients"]][["(Intercept)"]]
pi_treat_pt <- ols_port[["coefficients"]][["grupo_tratado"]] + ols_port[["coefficients"]][["(Intercept)"]]
pti_cont_pt <- ols_port[["coefficients"]][["(Intercept)"]] + ols_port[["coefficients"]][["ano_2007"]]
pti_treat_pt <- ols_port[["coefficients"]][["(Intercept)"]] + ols_port[["coefficients"]][["ano_2007"]] + ols_port[["coefficients"]][["grupo_tratado:ano_2007"]] + ols_port[["coefficients"]][["grupo_tratado"]] 

##Counterfactual=β0+β1+β2
counterfactual_pt <- ols_port[["coefficients"]][["(Intercept)"]] + ols_port[["coefficients"]][["ano_2007"]] + ols_port[["coefficients"]][["grupo_tratado"]] 

const_diff_pt = pi_treat_pt - pi_cont_pt
int_effect_pt <- pti_treat_pt - counterfactual_pt

time <- c('Before Intervention', 'Post-Intervention', 'Before Intervention', 'Post-Intervention', 'Before Intervention','Post-Intervention')
group <- c('Control', 'Control', 'Treatment', 'Treatment', 'Counterfactual','Counterfactual')
results <- c(pi_cont_pt, pti_cont_pt, pi_treat_pt, pti_treat_pt, pi_treat_pt,counterfactual_pt)

graph_pt <- data.frame(time, group, results)



pt <- ggplot(graph_pt, aes(x = time, y = results, group = group, color = group)) +
  geom_line(size = 2) +
  geom_point() +
  geom_segment(aes(x = 'Post-Intervention', xend = 'Post-Intervention', y = counterfactual_pt, yend = pti_treat_pt),
               linetype = "dashed", color = "black", size = 1) +
  geom_segment(aes(x = 'Post-Intervention', xend = 'Post-Intervention', y = counterfactual_pt, yend = pti_cont_pt),
               linetype = "dotted", color = "black", size = 1)  +
  labs(title = "Gráfico de DiD",
       subtitle = 'Português',
       x = "",
       y = "Resultados",
       color = "") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "top")

pt + annotate("text", x = c(2.15), y = c(185), 
             label = c("Efeito do\n tratamento") , color="black", 
             size=4 , fontface="bold")


#Passo 3: Estime o seguinte modelo por efeitos fixos:
#proficiência_it = α + β1 · tratamento_it + β3 · d.2007_t + β2_i + ε_i
fe_mat <- plm(profic_mat~grupo_tratado + ano_2007 + grupo_tratado*ano_2007, data = diff, index=c("escola", "ano"), model="within",na.action = na.exclude)

fe_pt <- plm(profic_port~grupo_tratado + ano_2007 + grupo_tratado*ano_2007, data = diff, index=c("escola", "ano"), model="within",na.action = na.exclude)
stargazer(fe_mat, fe_pt, ols_mat, ols_port, align=TRUE, type="text")


#Passo 4: Se utilizássemos o método de primeiras diferenças obteríamos resultados diferentes?

materia <- c('Matemática', 'Português')
pre_tratamento <- c(pi_treat_mat, pi_treat_pt)
pos_tratamento <- c(pti_treat_mat, pti_treat_mat)
contrafactual <- c(counterfactual_mat, counterfactual_pt)
diff <- c(int_effect_mat, int_effect_pt)

did <- data.frame(materia, pre_tratamento, pos_tratamento, contrafactual, diff)
#Passo 5: Suponha que você tivesse dados para o ano de 2003. Se adicionássemos os dados desse ano ao nosso banco e na equação (1) inseríssemos uma dummy para o ano de 2005 e a interação desta com grupo_tratado_i, qual seria a interpretação de um coeficiente β0 = 10 com erro-padrão, s.e.(β) = 3?











