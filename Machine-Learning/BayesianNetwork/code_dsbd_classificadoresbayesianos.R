### PACOTES UTILIZADOS ---------------------------------------
require(bnclassify)


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

#Data splitting -----------------------------------------------
set.seed(100)
l <- sample(1:nrow(asia),nrow(asia)*0.7)
dados.trein <- asia[ l,]
dados.teste <- asia[-l,]
View(asia)

#Naive Bayes
NB <- bnc('nb', 'B', dados.trein, smooth = 1)
pred <- predict(NB,dados.teste)
table(pred,dados.teste$B)


#TAN
TAN <- bnc('tan_cl', 'B', dados.trein, smooth = 1)
plot(TAN)
pred <- predict(TAN,dados.teste)
table(pred,dados.teste$B)

#KDB
KDB=kdb('B', dados.trein,k=2, kdbk=4) #estimando estrutura
KDB <- lp(KDB, dados.trein, smooth=1) #estimando as probabilidaes
plot(KDB)
pred <- predict(KDB,dados.teste)
table(pred,dados.teste$B)
