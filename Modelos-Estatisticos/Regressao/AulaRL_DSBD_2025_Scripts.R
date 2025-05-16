################################################################################
################################################################################
################################################################################
### Modelos estatísticos - Aula de modelos lineares
### Pacotes usados na aula.
require("ISLR")
require("ggplot2")
require("GGally")
require("leaps")
require("car")

options(device = X11)

################################################################################
### Carregamento e visualização inicial da base

data("College") ### Carregando a base
help("College") ### Acessando a documentação

head(College,10) ### Visualizando as dez primeiras linhas
dim(College) ### Acessando a dimensão da base
summary(College) ### Resumo das variáveis


################################################################################
### Vamos considerar Grad.Rate (taxa de formados) como a variável resposta
### na nossa análise. Começamos a análise com alguns gráficos.

ggplot(College, aes(x = Grad.Rate)) + geom_histogram(fill = 'grey', color = 'black') +
    theme_bw(base_size = 14)
### Distribuição das taxas de formados

ggplot(College, aes(x = Top10perc, y = Grad.Rate)) + geom_point(color = 'grey') +
    geom_smooth(method = "loess", se = FALSE, color = 'red') + 
    theme_bw(base_size = 14)
### Taxas de formados versus percentual de alunos entre os 10% melhores
### no ensino médio.

ggplot(College, aes(x = Outstate, y = Grad.Rate)) + geom_point(color = 'grey') +
    geom_smooth(method = "loess", se = FALSE, color = 'red') + 
    theme_bw(base_size = 14)
### Taxas de formados versus investimentos externos.

ggplot(College, aes(x = perc.alumni, y = Grad.Rate)) + geom_point(color = 'grey') +
    geom_smooth(method = "loess", se = FALSE, color = 'red') +
    theme_bw(base_size = 14)
### Taxas de formados versus porcentagens de ex-alunos contribuintes.

ggpairs(College) ### Matriz de gráficos de correlação.
ggcorr(College[,-1], label = TRUE, label_round = 2) ### Correlograma.

################################################################################
### Parte 1 - Ajuste dos modelos lineares. Comecemos com o caso de apenas uma 
### variável explicativa (no caso, perc.alumni)

### Para ajustar modelos lineares no R usamos a função lm. Vamos consultar
### a documentação da função.

help('lm')

### Ajuste da regressão linear simples (assumindo relação linear entre 
### a taxa de formados e o percentual de ex-alunos contribuintes)

ajuste1 <- lm(Grad.Rate ~ perc.alumni, data = College)
summary(ajuste1)
### O percentual de ex-alunos contribuintes tem efeito positivo, e 
### estatisticamente significativo na taxa de formados.

### Vamos visualizar o ajuste do modelo
ggplot(College, aes(x = perc.alumni, y = Grad.Rate)) + geom_point() +
    stat_smooth(method = "lm", color = 'red') +
    theme_bw(base_size = 14)

### Vamos investigar possível efeito quadrático do percentual de contribuíntes
### na taxa de formados. Para isso, adicionamos ao preditor o termo quadrático
### da variável explicativa, da seguinte forma:

ajuste2 <- lm(Grad.Rate ~ perc.alumni + I(perc.alumni^2), data = College)
summary(ajuste2)
### O termo quadrático é estatisticamente significativo, indicando que a
### relação entre as variáveis não é linear. Vamos dar um passo além, e
### incluir o termo de terceira ordem para o percentual de contribuintes
### (modelo cúbico).

ajuste3 <- lm(Grad.Rate ~ perc.alumni + I(perc.alumni^2) + I(perc.alumni^3), data = College)
summary(ajuste3)
### O termo de ordem cúbica não tem significância estatística. Vamos seguir
### a análise com o modelo quadrático.

### Vamos extrair alguns elementos do modelo ajustado
ajuste2$coefficients ### Estimativas dos parâmetros
ajuste2$residuals ### Resíduos ordeinários
ajuste2$fitted.values ### Valores ajustados pelo modelo
model.matrix(ajuste2) ### Matriz do modelo (matriz X)
vcov(ajuste2) ### Matriz de variâncias e covariâncias dos estimadores.

### Vamos visualizar o ajuste do modelo
ggplot(College, aes(x = perc.alumni, y = Grad.Rate)) + geom_point() +
    stat_smooth(method = "lm", formula = y ~ x + I(x^2), color = 'red') +
    theme_bw(base_size = 14)

### Extraindo os intervalos de confiança (95%) para os parâmetros
confint(ajuste2)

### vamos realizar algumas predições. Considere faculdades com os seguintes
### percentuais de ex-alunos contribuintes: 13, 28 e 45

new_data <- data.frame(perc.alumni = c(13, 28, 45))
predict(ajuste2, newdata = new_data, se.fit = TRUE)
### Estimativas pontuais e erros padrões (nota: erros padrões para a resposta
### média)

predict(ajuste2, newdata = new_data, interval = 'confidence') 
### Estimativas pontiais e intervalos de confiança (95%) para a resposta média.

predict(ajuste2, newdata = new_data, interval = 'prediction') 
### Estimativas pontiais e intervalos de confiança (95%) para a predição
### de uma nova observação.

### Vamos plotar as bandas de confiança e predição. Para isso, vamos preparar 
### uma base de dados com os valores ajustados e os ICs(95%) para a resposta
### média e para as predições.

pred_int <- predict(ajuste2, interval="prediction")
med_int <- predict(ajuste2, interval="confidence")
data_pred <- data.frame(pred_lwr = pred_int[,'lwr'], pred_upr = pred_int[,'upr'],
                        med_lwr = med_int[,'lwr'], med_upr = med_int[,'upr'],
                        fit = med_int[,'fit'], perc.alumni = College$perc.alumni,
                        Grad.Rate = College$Grad.Rate)

ggplot(data_pred, aes(x = perc.alumni, y = Grad.Rate))+
    geom_point() +
    geom_line(aes(y=med_lwr), color = "red", linetype = "dashed", linewidth = 1.25) +
    geom_line(aes(y=med_upr), color = "red", linetype = "dashed", linewidth = 1.25) +
    geom_line(aes(y=pred_lwr), color = "green", linetype = "dashed", linewidth = 1.25) +
    geom_line(aes(y=pred_upr), color = "green", linetype = "dashed", linewidth = 1.25) +
    geom_line(aes(y=fit), color = "black", linetype = "dashed", linewidth = 1.25) +
    theme_bw(base_size = 14)

################################################################################
### Diagnóstico do ajuste (análise de resíduos)

### Vamos produzir alguns gráficos para os resíduos

fit_aj2 <- fitted(ajuste2) ### Vetor de valores ajustados
resid_aj2 <- rstandard(ajuste2) ### Vetor de resíduos padronizados

data_fit <- data.frame(y = College$Grad.Rate, fit_aj2, resid_aj2)

ggplot(data_fit, aes(x=y, y=fit_aj2)) + geom_point() + stat_smooth(method="lm") +
    theme_bw(base_size = 14)
### Gráfico de valores observados versus valores ajustados. 

ggplot(ajuste2, aes(x=fit_aj2, y=resid_aj2)) + geom_point() +
    stat_smooth(method="loess") + geom_hline(yintercept=0, col="red", linetype="dashed") +
    theme_bw(base_size = 14) +
    xlab('Valores ajustados') +
    ylab('Resíduos')
### Gráfico de resíduos versus valores ajustados

qqPlot(ajuste2)
### Gráfico quantil-quantil para os resíduos

################################################################################
### Parte 2 - Regressão linear múltipla, incluindo todas as variáveis da
### base como explicativas (exceto a taxa de formação, que é a resposta)

ajuste_p2 <- lm(Grad.Rate ~., data = College)
### Ajuste da regressão linear múltipla. A especificação "~." indica que
### todas as demais variáveis da base devem ser incluídas como explicativas.
### A título de ilustração, se quiséssemos ajustar um modelo apenas com as
### variáveis "Private", "Apps" e "Accept":

ajuste_p2_ilustrativo <- lm(Grad.Rate ~ Private + Apps + Accept, data = College)

summary(ajuste_p2) 
### Resumo do ajuste. Observe as variáveis com efeito significativo e os respectivos sinais.
### Variáveis com p-valor (Pr(>|t|)) < 0.05 podem ser consideradas com efeito 
### significativo na taxa de formados. Desta forma, as variáveis com efeito
### significativo na taxa de formados são Private (Yes), Apps, Top25perc, Outstate,
### Room.Board e perc.alumni; Já as variáveis com efeito negativo na taxa de formados
### são P.Undergrad, Personal e Expend.


### Apenas para fins de discussão, vamos ajustar um modelo de regressão tendo como
### única variável explicativa o gasto por aluno.
ajuste_temp <- lm(Grad.Rate ~ Expend, data = College)
summary(ajuste_temp)

### Compare o efeito do gasto por aluno na taxa de formação produzida pelo
### modelo em que ajustamos também os efeitos das demais variáveis com o
### efeito produzido pelo modelo em que as demais variáveis não são consideradas.
### Qual a diferença? Como você a justifica?

par(mfrow = c(2,2))
plot(ajuste_p2, which = 1:4)
### Gráficos de resíduos.
### O gráfico do canto superior esquerdo indica que a variância dos resíduos varia
### um pouco com a média (fitted values), e que os resíduos apresentam algum desvio
### da normalidade (conforme o qqplot). Ainda, conforme o gráfico do canto inferior
### direito, algumas observações podem ser identificadas como possivelmente influentes,
### produzindo maiores valores para a distância de Cook.

### Embora tenhamos alguns indicativos (ainda que não tão severos) de falta de ajuste
### da regressão linear, para fins didáticos vamos seguir a análise com esse tipo
### de modelagem.


### Agora, vamos para a etapa de seleção de covariáveis. Para isso, vamos
### usar os recursos do pacote leaps. Vamos consultar a documentação da função
### regsubsets.

help("regsubsets")

all_reg <- regsubsets(Grad.Rate ~ ., method = "exhaustive", nvmax = 18, data = College)
### Explorando todas as regressões possíveis

plot(all_reg, scale="r2") ### Resultados baseados no R2
plot(all_reg, scale="adjr2") ### Resultados baseados no R2 ajustado
plot(all_reg, scale="bic") ### Resultados baseados no BIC
### Nesses gráficos avaliamos os resultados dos critérios para os melhores
### modelos ajustados com cada número de covariáveis.

s1 <- summary(all_reg, matrix.logical=TRUE)
s1 ### A matriz lógica permite identificar as covariáveis selecionadas 
### em cada modelo.

s1$rsq ### Valores de R2 para cada um dos modelos selecionados
s1$adjr2 ### Valores de R2 ajustado para cada um dos modelos selecionados
s1$bic ### Valores de BIC para cada um dos modelos selecionados

which.max(s1$adjr2)
coef(all_reg, id = 12)
### O modelo com doze covariáveis produziu maior valor de R2 ajustado.

which.min(s1$bic) 
coef(all_reg, id = 7)
### O modelo com sete covariáveis produziu menor valor de BIC.

which.max(s1$rsq) 
coef(all_reg, id = 17)
### O modelo com 17 covariáveis produziu menor valor de R2 (obviamente).

### Vamos produzir alguns gráficos usando os resultados desta análise.
n_cov <- 1:17

plot(n_cov, s1$bic, type = 'b', xlab = 'Número de covariáveis', 
     ylab = 'BIC', las = 1, pch = 20)
axis(1,1:17)

plot(n_cov, s1$adjr2, type = 'b', xlab = 'Número de covariáveis', 
     ylab = 'Adjusted R2', las = 1, pch = 20)
axis(1,1:17)

plot(n_cov, s1$rsq, type = 'b', xlab = 'Número de covariáveis', 
     ylab = 'R2', las = 1, pch = 20)
axis(1,1:17)


### Para finalizar, vamos aplicar os algoritmos de seleção do tipo
### stepwise. Primeiramente fixando k = 2, estamos definindo o AIC como critério de
### seleção, temos:


aj_full <- ajuste_p2
### Método backward
step_back_AIC <- step(aj_full, direction = "backward", data = College, k = 2)
summary(step_back_AIC)

### Método forward. Para o método forward devemos definir o escopo da seleção
### (menor e maior modelo). O menor seria o modelo nulo (apenas com o intercepto),
### enquanto o maior seria o modelo com todas as covariáveis.
aj_lower <- lm(Grad.Rate~1, data = College)

aj_upper <- lm(Grad.Rate~., data = College)
formula(aj_upper)

step_for_AIC <- step(aj_lower, direction = "forward", scope=formula(aj_upper), 
                     data = College, k = 2)
summary(step_for_AIC)

### Finalmente, o algoritmo que considera tanto exclusão quanto inclusão de
### covariáveis a cada passo
step_both_AIC <- step(aj_full, direction = "both", data = College, k = 2)
summary(step_both_AIC)

### Vamos comparar os ajustes.
data.frame(compareCoefs(step_back_AIC, step_for_AIC, step_both_AIC))


### Agora fixando k = log(n) (critério BIC):

### Método backward
step_back_BIC <- step(aj_full, direction = "backward", data = College, k = log(nrow(College)))
summary(step_back_BIC)

### Método forward. Para o método forward devemos definir o escopo da seleção
### (menor e maior modelo). O menor seria o modelo nulo (apenas com o intercepto),
### enquanto o maior seria o modelo com todas as covariáveis.
aj_lower <- lm(Grad.Rate~1, data = College)

aj_upper <- lm(Grad.Rate~., data = College)
formula(aj_upper)

step_for_BIC <- step(aj_lower, direction = "forward", scope=formula(aj_upper), 
                     data = College, k = log(nrow(College)))
summary(step_for_BIC)

### Finalmente, o algoritmo que considera tanto exclusão quanto inclusão de
### covariáveis a cada passo
step_both_BIC <- step(aj_full, direction = "both", data = College, k = log(nrow(College)))
summary(step_both_BIC)

### Vamos comparar os ajustes.
data.frame(compareCoefs(step_back_AIC, step_for_AIC, step_both_AIC,
                        step_back_BIC, step_for_BIC, step_both_BIC))
