################################################################################
################################################################################
################################################################################
### Modelos estatísticos - Quiz 2

#install.packages("faraway")

### Carregando os pacotes necessários para a análise.
require(ISLR)
require(statmod)
require(pROC)
require(car)
require(hnp)
require(tidyverse)
require(faraway)
require(pROC)

data("wbca")

??wbca

head(wbca)

wbca$Class <- factor(wbca$Class, levels = c("1", "0"))

### Separação da base de dados em dados de treino e dados de teste
dados_treino <- wbca[1:500, ]
dados_teste  <- wbca[501:681, ]


### Ajuste do modelo de regressão logística

ajuste <- glm(Class ~ Adhes + BNucl + Thick, data = dados_treino, family = binomial(link = 'logit'))
summary(ajuste)


#1 - Qual a chance estimada de tumor maligno para uma observação com as seguintes características:
nova_obs <- data.frame(
  Adhes = 4,
  BNucl = 7,
  Thick = 5
)

chance_maligno <- predict(ajuste, nova_obs)
exp(chance_maligno)

#2 - Qual a probabilidade estimada de tumor maligno para uma observação com as seguintes características:

prob_maligno <- predict(ajuste, nova_obs, type = "response")
print(prob_maligno)

# 3 - Considere a seguinte conjectura:
#Estima-se um que a chance de tumor maligno aumente K% para um aumento unitário em BNucl mantendo fixos Adhes e Thick.
#Qual das alternativas abaixo apresenta o valor de K?


(exp(coef(ajuste)["BNucl"]) - 1) * 100

# 4 - Considere a seguinte conjectura:
#Estima-se um que a chance de tumor maligno aumente K% para um aumento de duas unidades em Adhes mantendo fixos BNucl e Thick.
#Qual das alternativas abaixo apresenta o valor de K?

(exp(2 * coef(ajuste)["Adhes"]) - 1) * 100

#5 O número de variáveis explicativas que estão significativamente associadas ao tipo de tumor, ao nível de significância de 5%, é:

summary(glm(Class ~ ., data = dados_treino, family = binomial(link = 'logit')))

#6 - Qual das alternativas a seguir apresenta um intervalo de confiança (95%) para a chance estimada de tumor maligno para uma observação com as seguintes características:

p_link <- predict(ajuste, newdata = nova_obs, se.fit = TRUE)
ic_link <- cbind(p_link$fit - 1.96 * p_link$se.fit, p_link$fit + 1.96 * p_link$se.fit)
exp(ic_link)

#7 - Qual das alternativas a seguir apresenta um intervalo de confiança (95%) para a probabilidade estimada de tumor maligno para uma observação com as seguintes características:

exp(ic_link)/(exp(ic_link) + 1)

#8 - Com base na amostra de validação, a acurácia estimada do modelo, 
#ao classificar como tumor maligno as observações com probabilidade estimada maior que 0.50, 
#e como tumor benigno as observações com probabilidade estimada menor que 0.50, é igual a:

predicoes <- predict(ajuste, newdata = dados_teste, type = 'response')
tab_pred <- table(ifelse(predicoes < 0.5, 'Pred_No', 'Pred_Yes'), dados_teste$Class)
tab_pred
prop.table(tab_pred, 2)

r1 <- roc(dados_teste$Class, predicoes, plot=TRUE, ci=TRUE, ci.sp = TRUE)
coords(r1, x = 0.5, ret = c("sensitivity", "specificity", "accuracy"))

#9 - Com base na amostra de validação, a sensibilidade estimada do modelo, 
#ao classificar como tumor maligno as observações com probabilidade estimada maior que 0.50, 
#e como tumor benigno as observações com probabilidade estimada menor que 0.50, é igual a:

coords(r1, x = 0.5, ret = c("sensitivity", "specificity", "accuracy"))

#10 - Com base na amostra de validação, a especifidade estimada do modelo, 
#ao classificar como tumor maligno as observações com probabilidade estimada maior que 0.50, 
#e como tumor benigno as observações com probabilidade estimada menor que 0.50, é igual a:

coords(r1, x = 0.5, ret = c("sensitivity", "specificity", "accuracy"))


#11 - Com base na amostra de validação, a área sob a curva ROC produzida pelo modelo é igual a:

auc(r1)

#12 - Com base na amostra de validação, e usando o método de Youden, 
#a regra de classificação ótima ao se considerar as informações fornecidas 
#na sequência consiste em classificar um tumor como maligno caso a probabilidade estimada seja superior a K.

#Prevalência de tumor maligno igual a 0.20;
#Custo de classificar um tumor positivo como negativo é três vezes o custo de classificar um tumor negativo como positivo.
#Qual o valor de K?

coords(r1, 
       x = "best", 
       best.method = "youden", 
       best.policy = "cost", 
       cost.fp = 1, 
       cost.fn = 3, 
       prevalence = 0.20)