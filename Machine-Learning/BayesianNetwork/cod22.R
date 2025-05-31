### PACOTES UTILIZADOS ---------------------------------------
require(bnlearn) #www.bnlearn.com
require(Rgraphviz)
require(dplyr)
#instalação Rgraphviz
# install.packages("BiocManager")
# BiocManager::install("Rgraphviz")

#DATASET -----------------------------------------------------
data(asia)

# D (dyspnoea), a two-level factor with levels yes and no.
# T (tuberculosis), a two-level factor with levels yes and no.
# L (lung cancer), a two-level factor with levels yes and no.
# B (bronchitis), a two-level factor with levels yes and no.
# A (visit to Asia), a two-level factor with levels yes and no.
# S (smoking), a two-level factor with levels yes and no.
# X (chest X-ray), a two-level factor with levels yes and no.
# E (tuberculosis versus lung cancer/bronchitis), a two-level factor with levels yes and no.
View(asia)
nrow(asia)
str(asia)

#P(B)
asia %>%  select(B) %>% table() %>%  prop.table()


#P(S)
asia %>%  select(S) %>% table() %>%  prop.table()

#P(B|S)
asia %>%  filter(S=="yes") %>% 
  select(B) %>% table() %>%  prop.table()
asia %>%  filter(S=="no") %>% 
  select(B) %>% table() %>%  prop.table()

#Data splitting -----------------------------------------------
set.seed(1)
l <- sample(1:nrow(asia),nrow(asia)*0.75)
dados.trein <- asia[ l,]
dados.teste <- asia[-l,]

bl <- data.frame(from = c("B"), to = c("L"))

###Estimando Estrutura RB via hill climbling e score K2 -------
mestr_aic1 <- tabu(dados.trein,score='bic',blacklist = bl)
mestr_aic2 <- tabu(dados.trein,score='bic')
bnlearn::score(mestr_aic1,dados.trein,type='bic')
bnlearn::score(mestr_aic2,dados.trein,type='bic')

#estimação dos parâmetros
pest_aic1 <- bn.fit(mestr_aic1,dados.trein)
graphviz.plot(mestr_aic1)


## Estudo descritivo
#P(T)
dados.trein %>% 
  select(T) %>% table()  %>% prop.table()


#P(T|A=yes)
dados.trein %>% filter(A=='yes') %>% 
  select(T) %>% table()  %>% prop.table()

#P(T|A=no)
dados.trein %>% filter(A=='no') %>% 
  select(T) %>% table()  %>% prop.table()


bbn.fits <- boot.strength(dados.trein,
              R=200, algorithm = "tabu",m=nrow(dados.trein),
              algorithm.args = list(score="bic"))

mestr_aicB <- averaged.network(bbn.fits)
bnlearn::score(mestr_aicB,dados.trein,type='bic')

graphviz.plot(mestr_aicB)
