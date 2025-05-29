########################################################################
########################################################################
########################################################################
### Regressão para dados binários - modelo preditivo.
### Dados sobre concessão de crédito. O objetivo é modelar a variável 
### resposta (default), que indica se o indíviduo atrasou (Yes) ou não (No)
### o pagamento da fatura do cartão de crédito. As variáveis explicativas
### são as seguintes:

### income: renda anual;
### balance: saldo devedor no cartão de crédito no último período;
### student: Yes, para estudante; No, caso contrário.

### Carregando os pacotes necessários para a análise.
require(ISLR)
require(statmod)
require(pROC)
require(car)
require(hnp)
require(tidyverse)

### Carregamento e preparação dos dados.
data("Default")
help("Default")
summary(Default)
Default$income <- Default$income/1000 ### Renda em x$1.000.

### Podemos observar que a frequência de devedores é bastante inferior à de
### não devedores. 


########################################################################
########################################################################
########################################################################
### Análise exploratória.

ggplot(data = Default, aes(x = student, group = default, fill = default)) +
    geom_bar(stat = 'count', position = position_dodge())+
    theme_bw(base_size = 14)+
    theme(legend.position = 'bottom')+
    geom_text(aes( y=..count.., label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = 0.1L) ),
              stat="count", position=position_dodge(0.9), vjust=-0.5)

ggplot(data = Default, aes(x = default, y = balance, fill = default)) +
    geom_boxplot()+
    theme_bw(base_size = 14)+
    theme(legend.position = 'none')

ggplot(data = Default, aes(x = default, y = income, fill = default)) +
    geom_boxplot()+
    theme_bw(base_size = 14)+
    theme(legend.position = 'none')

### Separação da base de dados em dados de treino e dados de teste
set.seed(2024)
w <- sample(1:nrow(Default), size = 7000)

dados_treino <- Default[w,]
dados_teste <- Default[-w,]

########################################################################
########################################################################
########################################################################
### Ajuste do modelo de regressão logística

ajuste <- glm(default ~ ., data = dados_treino, family = binomial(link = 'logit'))
options(scipen = 5)
summary(ajuste)

### Algumas interpretações:
### A log-chance de default aumenta em 0.0057 para $1 a mais no
### balanço. De maneira equivalente, a chance de não pagamento fica
### multiplicada por exp(0.0057) = 1.006 para $1 a mais no balanço,
### ou ainda, por exp(100*0.0057) = 1.768 para $100 a mais no balanço
### (fixados os valores das demais covariáveis). 

### Alog-chance de não pagamento para estudantes é 0.632 menor do que para não
### estudantes, ou ainda, a chance de não pagamento fica multiplicada
### por exp(-0.631) = 0.53 para estudantes em relação a não estudantes,
### fixados os valores das demais covariáveis. 

### O efeito de renda, ajustado pelas outras duas variáveis, não é significativo.

########################################################################
### Análise de resíduos

### Vamos dar sequência à análise com o diagnóstico do ajuste.
par(mfrow = c(2,2))
plot(ajuste)
### Os gráficos de resíduos têm comportamento bastante atípico, mas característico
### da análise de dados binários, devido aos empates. Para um diagnóstico mais
### adequado, vamos usar os resíduos quantílicos aleatorizados, disponíveis
### no pacote statmod, e os gráficos meio-normais com envelope simulado,
### disponíveis no pacote hnp.

residuos <- qres.binom(ajuste)
ajustados <- predict(ajuste)

par(las = 1, mar = c(5,4.5,2,2), mfrow = c(1,2), cex = 1.2)
plot(residuos ~ ajustados, col = 'blue', xlab = 'Valores ajustados', ylab = 'Resíduos')
lines(lowess(residuos ~ ajustados), col = 'red', lwd = 2)
qqnorm(residuos, col = 'blue', main = '')
qqline(residuos, lty = 2)

### Os resíduos apresentam dispersão aleatória, variância aprox. constante e 
### distribuição normal. O modelo parece estar bem ajustado.

par(las = 1, mar = c(5,4.5,2,2), cex = 1.4)
hnp(ajuste)
### O padrão para um ajuste adequado é os resíduos (pontos) dispostos no
### interior do envelope (linhas) simulado. Novamente temos um indicativo
### de que o modelo está bem ajustado.

########################################################################
########################################################################
########################################################################
### Inferência estatística e redefinição do modelo

### Como o efeito de renda não se mostrou significativo, vamos removê-lo
### do modelo.
ajuste2 <- update(ajuste, ~.-income)
summary(ajuste2)

### Agora, vamos usar o modelo ajustado para fins de predição. Antes de 
### utilizar a base de validação, vamos fazer predição para algus dados
### adicionais.
novos_dados <- data.frame(student = rep(c('Yes', 'No'), times = 3),
                          balance = c(500, 500, 1000, 1000, 1750, 1750))
novos_dados
### Base para predição.

predict(ajuste2, newdata = novos_dados)
### Predição na escala  da log-chance de default

exp(predict(ajuste2, newdata = novos_dados))
### Predição na escala  da chance de default

predict(ajuste2, newdata = novos_dados, type = 'response')
### Predição na escala da probabilidade de default

### Agora, vamos fazer intervalos de confiança (95%) para a probabilidade
p_link <- predict(ajuste2, newdata = novos_dados, se.fit = TRUE)
### Predições na escala da log-chance com os erros padrões associados.

ic_link <- cbind(p_link$fit - 1.96 * p_link$se.fit, p_link$fit + 1.96 * p_link$se.fit)
ic_link
### Intervalos de confiança (95%) para a log-chance de default.

exp(ic_link)/(exp(ic_link) + 1)
### Intervalos de confiança (95%) para a probabilidade de default.

########################################################################
########################################################################
########################################################################
### Validação do modelo usando a base de teste.

### na sequência, vamos retomar a amostra de validação para avaliar a 
### capacidade preditiva do modelo.

predicoes <- predict(ajuste2, newdata = dados_teste, type = 'response')
### Probabilidades estimadas de default para os indivíduos da base de validação.

hist(predicoes, breaks = 20, main = '')

### Vamos ver como ficaria o resultado da predição se adotássemos o ponto
### de corte p=0.5 para predição, isto é, classificando como não pagadores
### os indivíduos com probabilidade estimada superior a 0.5 e como pagadores
### aqueles com probabilidade inferior a 0.5.

tab_pred <- table(ifelse(predicoes < 0.5, 'Pred_No', 'Pred_Yes'), dados_teste$default)
tab_pred
prop.table(tab_pred, 2)

### A regra de classificação baseada no ponto de corte p = 0.5 tem elevada
### especificidade (0.995), mas baixa sensibilidade (0.361). Neste
### problema, em particular, sensibilidade (identificar não pagadores)
### deve ser mais importante que especificidade (identificar pagadores).
### Desta forma, poderíamos considerar um valor menor para o ponto de corte,
### visando aumentar a sensibilidade do modelo. Vejamos como ficariam os
### resultados para p = 0.1.

tab_pred <- table(ifelse(predicoes < 0.1, 'Pred_No', 'Pred_Yes'), dados_teste$default)
tab_pred
prop.table(tab_pred, 2)

### Neste cenário, a especificidade é ligriramente reduzida para 0.946. 
### Em contrapartida, a sensibilidade é aumentada para 0.771.
### Nesse sentido, precisamos explorar adequadamente a capacidade preditiva
### do modelo e buscar regras de classificação alternativas. Vamos usar 
### os recurdos do pacote pRoc.

r1 <- roc(dados_teste$default, predicoes, plot=TRUE, ci=TRUE, ci.sp = TRUE)
r1
### A área sob a curva ROC é uma medida de qualidade preditiva do modelo.
### Valores próximos de 1 indicam modelos com elevada capacidade preditiva,
### enquanto valores próximos de 0.5 indicam modelos cujas predições são
### realizadas ao acaso. Mais do que interpretá-lo, é um indicador importante
### para comparação da performance de diferentes modelos preditivos aplicados
### a uma base de dados.

plot(r1, print.thres = c(0.001, 0.005, 0.01, 0.02, 0.03, 0.04, seq(0.05,0.95,0.05)),
     print.thres.pattern.cex = 0.8)
### Curva ROC. O valor que aparece fora dos parênteses é o ponto de corte.
### No interior temos a sensibilidade e a especificidade correspondentes,
### respectivamente. Pontos de corte posicionados no canto superior esquerdo
### são aqueles que combinam maior sensibilidade e especificidade. Pontos
### de corte em torno de 0.05 produzem regras de classificação que conjugam
### elevadas sensibilidade e especificidade. No entanto, aspectos operacionais
### e do relacionamento com os clientes também devem ser levados em consideração
### ao se estabelecer a regra de classificação.

coords(r1, x = 0.01, ret = c("sensitivity", "specificity", "accuracy"))
### Sensibilidade, especificidade e acurácia para o ponto de corte p = 0.01.

coords(r1, x = 0.05, ret = c("sensitivity", "specificity", "accuracy"))
### Sensibilidade, especificidade e acurácia para o ponto de corte p = 0.05.

### Agora, vamos identificar a melhor regra de decisão (ponto de corte)
### associada a diferentes custos de máclassificação. No argumento
### "best.weights=c(a, b)", em que "a" representa o custo de um falso negativo
### relativo a um falso positivo e "b" a prevalência (proporção de sucessos)
### na população.

### Vamos lembrar que, neste exemplo, falso negativo corresponde a classificar
### como pagador um não pagador. A prevalência de maus pagadores nós vamos 
### fixar em 0.033, que é a prevalência verificada na base.

coords(r1, x = "best", best.method = "youden", best.weights=c(1, 0.033))
### Custos iguais.

coords(r1, x = "best", best.method = "youden", best.weights=c(2, 0.033))
### O custo do falso negativo é duas vezes o do falso positivo.

coords(r1, x = "best", best.method = "youden", best.weights=c(5, 0.033))
### O custo do falso negativo é cinco vezes o do falso positivo.

coords(r1, x = "best", best.method = "youden", best.weights=c(20, 0.033))
### O custo do falso negativo é vinte vezes o do falso positivo.

### Na área de concessão de crédito, é usual se trabalhar com o chamado
### "escore de crédito", que é 100*P(pagador|x). Ou seja:

cred_escores <- 100*(1-predicoes)
head(cred_escores, 20)
hist(cred_escores, main = '')