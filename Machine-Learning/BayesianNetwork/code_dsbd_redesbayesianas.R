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
l <- sample(1:nrow(asia),nrow(asia)*0.7)
dados.trein <- asia[ l,]
dados.teste <- asia[-l,]

###Estimando Estrutura RB via hill climbling e score K2 -------
mestr_k2 <- hc(dados.trein,score='k2')
plot(mestr_k2)
graphviz.plot(mestr_k2)
mestr_k2


#Estimando os parâmetros -------------------------------------- 
mpara_k2 <- bn.fit(mestr_k2,dados.trein)

par(bg="white")
graphviz.chart(mpara_k2, 
               type="barprob",
               draw.labels =TRUE,
               grid=TRUE,bg="white")

score(mestr_k2,dados.teste,type="k2")

#Predição -----------------------------------------------------
pred_k2 <- predict(mpara_k2, 'L',
                data = dados.teste, 
                method = "bayes-lw")
table(pred_k2,dados.teste$L)

pred_k2 <- predict(mpara_k2, 'E',
                   data = dados.teste, 
                   method = "bayes-lw")
table(pred_k2,dados.teste$E)




###Estimando Estrutura RB via tabu-search e score K2 --------
mestr_tb <- tabu(dados.trein,score='k2')
plot(mestr_tb)
par(bg="white")
graphviz.plot(mestr_tb)
mestr_tb


#Estimando os parâmetros -------------------------------------- 
mpara_tb <- bn.fit(mestr_tb,dados.trein)
par(bg="white")
graphviz.chart(mpara_tb, layout = "dot",
               type="barprob",
               draw.labels =TRUE,
               grid=TRUE,bg="white")

score(mestr_tb,dados.teste,type="k2")

#Predição -----------------------------------------------------
pred_tb <- predict(mpara_tb, 'L',
                   data = dados.teste, 
                   method = "bayes-lw")
table(pred_tb,dados.teste$L)

pred_tb<- predict(mpara_tb, 'E',
                   data = dados.teste, 
                   method = "bayes-lw")
table(pred_tb,dados.teste$E)


###Estimando Estrutura RB via pc.stable ----------------------
mestr_pc <- pc.stable(dados.trein)
par(bg="white")
plot(mestr_pc)
graphviz.plot(mestr_pc)
mestr_pc
mestr_pc <- set.arc(mestr_pc,from="S",to="L")
mestr_pc <- set.arc(mestr_pc,from="S",to="B")
mestr_pc <- set.arc(mestr_pc,from="B",to="D")
graphviz.plot(mestr_pc)

#Estimando os parâmetros -------------------------------------- 
mpara_pc <- bn.fit(mestr_pc,dados.trein)

graphviz.chart(mpara_pc, layout = "dot",
               type="barprob",
               draw.labels =TRUE,
               grid=TRUE)

score(mestr_pc,dados.teste,type="k2")

#Predição -----------------------------------------------------
pred_pc <- predict(mpara_pc, 'L',
                   data = dados.teste, 
                   method = "bayes-lw")
table(pred_pc,dados.teste$L)

pred_pc<- predict(mpara_pc, 'E',
                  data = dados.teste, 
                  method = "bayes-lw")
table(pred_pc,dados.teste$E)


#### Além do critério preditivo, pode-se comparar
# os scores das 3 estimações 
# quanto maior o valor de k2,
# maior o indicativo de qualidade de ajuste.
# Existem outras diversas métricas de qualidade
score(mestr_k2,dados.teste,type="k2")
score(mestr_tb,dados.teste,type="k2")
score(mestr_pc,dados.teste,type="k2")


