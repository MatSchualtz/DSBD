################################################################################
################################################################################
################################################################################
### Modelos estatísticos - Quiz 1

#install.packages('ISLR')

require("ISLR")
require("MASS")
require("ggplot2")
require("GGally")
require("leaps")
require("car")


################################################################################
### Carregamento e visualização inicial da base

help("Carseats")

df <- Carseats
head(df,10)
dim(College) 
str(df)
summary(College)


################################################################################
### Vamos considerar. Sales (Unidades vendidas em milhares) como a variável resposta
### na nossa análise. Começamos a análise com alguns gráficos.


ggplot(df, aes(x = Sales)) + geom_histogram(fill = 'grey', color = 'black') +
  theme_bw(base_size = 14)


### Distribuição das Unidades vendidas

ggplot(df, aes(x = CompPrice, y = Sales)) + geom_point(color = 'grey') +
  geom_smooth(method = "loess", se = FALSE, color = 'red') + 
  theme_bw(base_size = 14)
### Unidades vendidas versus Preço da concorrência

ggplot(df, aes(x = Price, y = Sales)) + geom_point(color = 'grey') +
  geom_smooth(method = "loess", se = FALSE, color = 'red') + 
  theme_bw(base_size = 14)
### Unidades vendidas versus Preços.

ggplot(df, aes(x = Advertising, y = Sales)) + geom_point(color = 'grey') +
  geom_smooth(method = "loess", se = FALSE, color = 'red') +
  theme_bw(base_size = 14)
### Unidades vendidas versus Gastos com Publicidade.

ggpairs(df) ### Matriz de gráficos de correlação.
ggcorr(df, label = TRUE, label_round = 2) ### Correlograma.

################################################################################
### Ajuste dos modelos lineares. 

ajuste_p1 <- lm(Sales ~., data = df)

summary(ajuste_p1)



# Questão 1 - Qual o número de unidades vendidas ajustado pelo modelo considerando os seguintes valores para as variáveis explicativas:

'''
CompPrice = 125
Income = 80
Advertising = 0
Population = 300
Price = 125
ShelveLoc = ‘Good’
Age = 60
Education = 15
Urban = ‘No’
US = ‘Yes’

'''

new_customer <- data.frame(
  CompPrice = 125,
  Income = 80,
  Advertising = 0,
  Population = 300,
  Price = 125,
  ShelveLoc = factor("Good", levels = levels(Carseats$ShelveLoc)),
  Age = 60,
  Education = 15,
  Urban = factor("No", levels = levels(Carseats$Urban)),
  US = factor("Yes", levels = levels(Carseats$US))
)

predict(ajuste_p1, newdata= new_customer)


# Questão 2 - Quais os valores do resíduo ordinário e resíduo studentizado para a primeira observação da base?

ajuste_p1$residuals[1] # Resíduo ordinário
studres(ajuste_p1)[1] # Resíduo Studentizado

# Questão 3 - Considere a seguinte conjectura:

'''
Estima-se um aumento médio de K unidades vendidas (em milhares) para um aumento de 10 unidades em Comprice, mantendo-se fixos os valores das demais variáveis explicativas}

Qual é o valor de K?

'''

coef(ajuste_p1)['CompPrice'] * 10


# Questão 4 - Considere a seguinte conjectura:

'''

Estima-se uma diferença de K no número médio de unidades vendidas (em milhares) para ShelveLoc=Good em relação a ShelveLoc=Bad, 
mantendo fixos os valores das demais variáveis explicativas.

Qual é o valor de K?

'''

coef(ajuste_p1)['ShelveLocGood']


# Questão 5 - Considere a seguinte conjectura:

'''

Estima-se uma diferença de K no número médio de unidades vendidas (em milhares) para ShelveLoc=Good em relação a ShelveLoc=Medium, 
mantendo fixos os valores das demais variáveis explicativas.

Qual é o valor de K?

'''

coef(ajuste_p1)['ShelveLocGood'] - coef(ajuste_p1)['ShelveLocMedium']

# Questão 6 - Qual das alternativas a seguir apresenta um intervalo com 99% de confiança para o parâmetro (𝛽) associado à variável 𝙰𝚍𝚟𝚎𝚛𝚝𝚒𝚜𝚒𝚗𝚐?

confint(ajuste_p1)

# Questão 7 - Qual das alternativas a seguir apresenta um intervalo com 95% de confiança para o número esperado (médio) de itens vendidos considerando lojas com as seguintes características:

'''
CompPrice = 125
Income = 80
Advertising = 0
Population = 300
Price = 125
ShelveLoc = ‘Good’
Age = 60
Education = 15
Urban = ‘No’
US = ‘Yes’

'''

predict(ajuste_p1, newdata = new_customer, interval = "confidence", level = 0.95)


# Questão 8 - Qual das alternativas a seguir apresenta o número de variáveis com efeito significativo na resposta ao nível de significância de 5%?

summary(ajuste_p1)

# Questão 9 - Qual o número de variáveis remanescentes no modelo ao se aplicar o algoritmo backward considerando (I) AIC e (II) BIC como critérios, respectivamente?

step(ajuste_p1, direction = "backward", data = df, k = 2) #AIC
step(ajuste_p1, direction = "backward", data = df, k = log(nrow(df))) #BIC


# Questão 10 - Qual o número de variáveis remanescentes no modelo ao se aplicar o algoritmo forward considerando (I) AIC e (II) BIC como critérios, respectivamente?

aj_lower <- lm(Sales~1, data = df)

aj_upper <- lm(Sales~., data = df)
formula(aj_upper)

step(aj_lower, direction = "forward", scope=formula(aj_upper), data = df, k = 2) #AIC
step(aj_lower, direction = "forward", scope=formula(aj_upper), data = df, k = log(nrow(df))) #BIC


# Questão 11 - Considere a seguinte conjectura:

'''

Aproximadamente K% da variação das quantidades vendidas pelas lojas é explicada pelo modelo originalmente ajustado.

Qual é o valor de K?

'''

summary(ajuste_p1)$r.squared
