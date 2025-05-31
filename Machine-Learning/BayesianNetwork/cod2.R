### PACOTES UTILIZADOS ---------------------------------------
require(bnlearn) #www.bnlearn.com
require(Rgraphviz)
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

#Data splitting -----------------------------------------------
set.seed(100)
l <- sample(1:nrow(asia),nrow(asia)*0.8)
dados.trein <- asia[ l,]
dados.teste <- asia[-l,]

###Estimando Estrutura RB via hill climbling e score K2 -------
mestr_k2 <- hc(dados.trein,score='aic')
mestr_k2$nodes
plot(mestr_k2)
graphviz.plot(mestr_k2)
mestr_k2

mest_pc <- pc.stable(dados.trein)
graphviz.plot(mest_pc)
