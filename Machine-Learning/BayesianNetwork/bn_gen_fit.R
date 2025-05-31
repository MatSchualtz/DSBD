#BiocManager::install("Rgraphviz")
#install.packages('gRain')
require(bnlearn)
require(qgraph)
require(GGally)
require(gRain)

## BN STRUCTURE
est <- model2network("[A][B][C|A][D|B][E|C:D]")


## BN PARAMETERS
cptA =matrix(c(0.20, 0.80), ncol = 2, dimnames = list(NULL, c("FALSE", "TRUE")))
cptB =matrix(c(0.75, 0.25), ncol = 2, dimnames = list(NULL, c("FALSE", "TRUE")))
cptC = matrix(c(0.9, 0.1, 
                0.25, 0.75),ncol=2,nrow=2)
dimnames(cptC)=list("C"=c("TRUE","FALSE"),
                    "A"=c("TRUE","FALSE"))
cptD = matrix(c(0.20, 0.80, 
                0.75, 0.25),ncol=2,nrow=2)
dimnames(cptD)=list("D"=c("TRUE","FALSE"),
                    "B"=c("TRUE","FALSE"))
cptE = c(0.3, 0.7, 
         0.4, 0.6, 
         0.3, 0.7, 
         0.2, 0.8)
dim(cptE) = c(2, 2, 2)
dimnames(cptE) = list("E" = c("TRUE", "FALSE"), 
                      "C" = c("TRUE", "FALSE"),
                      "D" = c("TRUE", "FALSE"))
                                                     
dfit = custom.fit(est, 
                  dist = list(A = cptA, 
                              B = cptB, 
                              C = cptC,
                              D = cptD,
                              E = cptE))

## BN GRAPH
graphviz.chart(dfit,layout="dot",
               type="barprob",
               bar.col = "tomato")

## BN ARTIFICIAL DATA
set.seed(1)
dados = rbn(dfit, 5000)
#View(dados)

## VIA PONTUAÇÃO
mestr <- tabu(dados,score='bic')

bn.fit(mestr,dados)
graphviz.plot(mestr)

bnlearn::score(mestr,dados,
               type='bic')

mestrD <- drop.arc(mestr,from="A", to="C")
bnlearn::score(mestrD,dados,
               type='bic')


bootbn <- boot.strength(dados,
                        R=500,
                        algorithm = "tabu",
                        m=nrow(dados),
                        algorithm.args = list(score="bic"))


plot(bootbn)

qgraph::qgraph(bootbn,
               edge.labels=T,
               color="tomato")

bootbn[(bootbn$strength  > 0.85) & 
         (bootbn$direction >= 0.5), ]

mestrB <- averaged.network(bootbn, threshold = 0.8)



strength.plot(mestrB, bootbn, 
              shape = "rectangle", 
              main = "")



bnlearn::score(mestrB,dados,type='bic')

graphviz.plot(mestrB)



am_raw <- amat(x = mestrB)
require(GGally)
ggnet2(net = am_raw, node.size = 10, 
       edge.size = 0.35, 
       edge.color = "black", label = TRUE,
       label.size = 3, 
       arrow.gap = 0.055,arrow.size = 5, color = "tomato")
