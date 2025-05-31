########################################################################
########################################################################
########################################################################
### Regressão para dados de contagens. Informações referentes a 500 portadores
### de seguro de automóvel de uma particular seguradora. Foram filtrados
### apenas os segurados cujas apólices têm mais de cinco anos. As variáveis
### são as seguintes:

# idade: idade do segurado (em anos);

# sexo: Masc para masculino e Fem para feminino;

# usop: uso principal do veículo. Cidade para uso urbano e Estrada para 
# uso rodoviário;

# anosest: escolaridade do segurado, em anos de estudo;

# claims: número de sinistros produzidos pelo segurado nos últimos cinco
# anos.

### O objetivo aqui é modelar a frequência de sinistros em função das demais
### covariáveis.

### Carregando os pacotes necessários para a análise.
require(rattle.data)
require(statmod)
require(ISLR)
require(car)
require(hnp)
require(pscl)
require(coefplot)
require(effects)
require(sandwich)
require(ggResidpanel)

options(device = 'x11')

########################################################################
########################################################################
########################################################################
### Importação dos dados e análise exploratória.

dados <- read.csv2('sinistros.csv', stringsAsFactors = TRUE)

head(dados, 10) ### Dez primeiras linhas da base.
summary(dados) ### Algumas descritivas dos dados.

### Análise descritiva

par(las = 1, mar = c(5,4.5,2,2), cex = 1.4)
plot(table(dados$claims), col = 'blue', lwd = 5, xlab = 'Número de sinistros',
     ylab = 'Frequência', xaxt = 'n')
axis(1, 0:9) 
### Distribuição de frequências para o número de sinistros.

d1 <- with(dados, data.frame(table(sexo, factor(claims, levels = 0:9))))
names(d1) <- c("sexo","claims","Freq")
ggplot(data = d1, aes(x = claims, y = Freq)) + geom_bar(stat = "identity")+ 
    facet_wrap(~ sexo) + theme_bw(base_size = 14) + ylab("Frequência") + xlab('Número de sinistros')

d2 <- with(dados, data.frame(table(usop, factor(claims, levels = 0:9))))
names(d2) <- c("usop","claims","Freq")
ggplot(data = d2, aes(x = claims, y = Freq)) + geom_bar(stat = "identity")+ 
    facet_wrap(~ usop) + theme_bw(base_size = 14) + ylab("Frequência") + xlab('Número de sinistros')

ggplot(data = dados, aes(x = anosest, y = claims)) + geom_jitter() +
    theme_bw(base_size = 14) + ylab("Número de sinistros") + xlab('Anos de estudo') +
    geom_smooth(method = 'loess')

ggplot(data = dados, aes(x = idade, y = claims)) + geom_jitter() +
    theme_bw(base_size = 14) + ylab("Número de sinistros") + xlab('Idade') +
    geom_smooth(method = 'loess')

########################################################################
########################################################################
########################################################################

### Ajuste de um modelo linear com erros normais
ajuste1 <- lm(claims ~ idade + sexo + usop + anosest, data=dados)
par(mfrow=c(2,2), cex = 1.4, las = 1)
resid_panel(ajuste1, plots = c("resid", "qq", "ls", "cookd"), qqbands = TRUE, nrow = 2)
### Os resíduos são claramente heterocedásticos (variância aumenta conforme
### a média) e têm distribuição assimétrica. O modelo linear não se ajusta
### bem aos dados.

########################################################################
########################################################################
########################################################################
### Ajuste de um modelo log-linear com resposta poisson

ajuste2 <- glm(claims ~ . ,family = poisson(link = 'log'), data = dados)
coefficients(ajuste2) 
### Estimativas dos parâmetros de regressão.

### Diagnóstico do ajuste.

par(mfrow=c(2,2), cex = 1.4, las = 1)
resid_panel(ajuste2, plots = c("resid", "qq", "ls", "cookd"), qqbands = TRUE, nrow = 2)

### Os resíduos utlizados ao plotar um objeto da classe glm são os resíduos
### componentes da deviance. Nem sempre esses resíduos têm boa aproximação
### com a distribuição normal se o modelo ajustado estiver correto. Isso
### ocorre, particularmente, para dados binários ou de contagens. No entanto,
### É possível notar comportamento mais aceitável (variância mais homogênea,
### melhor aproximação com a distribuição Normal) em relação ao que foi 
### verificado para o modelo linear.

### Para uma melhor avaliação do comportamento dos resíduos, vamos proceder
### com a análise dos resíduos quantílicos aleatorizados normalizados e a obtenção do
### gráfico meio normal com envelopes simulados.

residuos <- qres.pois(ajuste2)
ajustados <- predict(ajuste2)

par(las = 1, mar = c(5,4.5,2,2), mfrow = c(1,2), cex = 1.2)
plot(residuos ~ ajustados, col = 'blue', xlab = 'Valores ajustados', ylab = 'Resíduos')
lines(lowess(residuos ~ ajustados), col = 'red', lwd = 2)
qqnorm(residuos, col = 'blue', main = '')
qqline(residuos, lty = 2)
### Os resíduos estão dispersos de forma aleatória, sem padrões sistemáticos
### ou evidência de variância não constante. Adicionalmente, o gráfico quantil-
### quantil indica boa aproximação com a distribuição normal.

par(las = 1, mar = c(5,4.5,2,2), cex = 1.2)
hnp(ajuste2)
### O gráfico half-normal plot com envelopes simulados indica que o modelo se ajusta 
### satisfatoriamente aos dados.


### Vamos extrair mais alguns elementos produzidos pela função glm:

head(model.matrix(ajuste2)) 
### Matriz do modelo.

fitted(ajuste2) 
### Valores ajustados pelo modelo, para os 500 indivíduos da base, na escala 
### da média.

predict(ajuste2) 
### Valores ajustados pelo modelo, para os 500 indivíduos da base, na escala 
### da log-média.

predict(ajuste2, type = 'response') 
### Valores ajustados pelo modelo, para os 500 indivíduos da base, na escala 
### da média.

summary(ajuste2) 
### Resumo do modelo ajustado. Vamos interpretar os efeitos significativos (p<0.05)

### Estima-se que a frequência média de sinistros seja multiplicada por exp(-0.058) = 0.943
### para cada ano a mais de idade, fixados os valores das outras variáveis.

### Estima-se que a frequência média de sinistros seja multiplicada por exp(0.257) = 1.293
### vezes para uso principal na estrada em relação a cidade, fixados os valores 
### das outras variáveis.


### Inferência e predições.

summary(ajuste2)
### Os efeitos ajustados de sexo e anos de estudo não são significativos. Vamos 
### removê-los um a um do modelo, caso não significativos.

ajuste3 <- update(ajuste2, ~.-sexo)
summary(ajuste3)

ajuste4 <- update(ajuste3, ~.-anosest)
summary(ajuste4)

### Intervalos de confiança para os parâmetros de regressão.
confint(ajuste4)

### Exponenciando os limites, para uma escala em que são interpretáveis:
exp(confint(ajuste4))

### Podemos pedir intervalos com outros níveis de confiança alterando o 
### argumento level.
confint(ajuste4, level = 0.99)
exp(confint(ajuste4, level = 0.99))

### Agora, vamos estimar o número médio de sinistros para alguns perfis
### de segurados. Primeiro, para segurados de 30 anos que fazem uso principal 
### na estrada.

predict(ajuste4, newdata = data.frame(idade = 30, usop = 'Estrada'))
### Por default, o R retorna a predição na escala da log-média. Para obter a
### estimativa da média devemos aplicar a inversa da função de ligação:

exp(1.137746)

### ou pedir a predição direto na escala da média (resposta):
predict(ajuste4, newdata = data.frame(idade=30, usop = 'Estrada'), type = 'response')

### Podemos realizar predições para toda uma base de novos indivíduos. Uma
### pequena ilustração:

dpred <- data.frame(idade = c(30,30,60,60),
                    usop = c('Estrada','Cidade','Estrada','Cidade'))

mu_est <- predict(ajuste4, newdata = dpred, type = 'response')
dpred$mu <- mu_est
rownames(dpred) <- c('Perfil 1', 'Perfil 2', 'Perfil 3', 'Perfil 4')
dpred
### estimativas para o número esperado de sinistros em cinco anos para os
### quatro perfis de segurados.

### Gráficos de efeitos
plot(allEffects(ajuste4)) ### Escala da log-média.
plot(allEffects(ajuste4), type = 'response') ### Escala da média.
