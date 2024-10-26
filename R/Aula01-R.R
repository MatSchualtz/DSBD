# FUNÇÕES BÁSICAS

## Comentário
1 + 1 
2 * 2 
## Comentario 2
1 + 3 

## atribuição com <- e =
x <- 10
y = 10

## Listar objetos do Ambiente com ls
ls()

## Deletar variavel do ambiente rm()

rm(x)
ls()

rm(list = ls()) #Limpar todos os elementos da memória 

## Localizar todos os Ambientes de Trabalho carregado 
search()


## Criar uma pasta 

dir.create("data")

## Mostrar os arquivos que tenho na pasta 
list.files()
dir()

## Instalar bibliotecas 
install.packages("ggplot2")

## Importar lib
library(ggplot2)
require(ggplot2)
ls('package:ggplot2')
?theme

# Operações Aritiméticas 

## Básicas 

1 + 1 # Adição
2 - 1 # Subtração
3 * 2 # Multiplicação
4 / 2 # Divisão
5 ^ 2 # Potenciação
5 %% 2 # Resto da divisão
5 %/% 2 # Divisão inteira

## Logarítimo 
log(10) # Logarítimo natural
exp(10) # Exponencial
log10(100) # Logarítimo base 10
log2(10) # Logarítimo base 2
log(100, base = 8) # Log. base 8

## Funções Trigonométricas

sin(0) # Seno
cos(0) # Cosseno
tan(0) # Tangente
asin(0) # Arco seno
acos(0) # Arco cosseno
atan(0) # Arco tangente

## Arrerondamento

round(pi, digits = 2) # Arredondamento
ceiling(pi) # Teto
floor(pi) # Piso
trunc(pi) # Truncamento


## Operações Lógicas 

### Comparativos
1 == 1 # Igual
1 != 2 # Diferente
1 > 2 # Maior
1 < 2 # Menor
1 >= 1 # Maior ou Igual
2 >= 1 # Menor ou Igual

a <- 1
b <- 2
a <= b

### Operadores Lógicos

(1 == 1) & (2 == 2) # E / AND
(1 == 1) | (2 == 3) # OU / OR
!(1 == 2) # NÃO / NOT

### Strings

"R" == "r" # Comparação de strings
"a" < "b" # Ordem Alfanumérica
"1" < "2" # Ordem Alfanumérica


#Vetores

numeros <- c(1, 2, 3, 4, 5)
numeros[2]

letras <- c("a", "b", "c", "d", "e")
letras

logicos <- c(TRUE, FALSE, TRUE, FALSE)
logicos

## Coerção
vetor <- c(numeros, letras, logicos)
vetor

class(numeros)
class(letras)
class(logicos)
class(vetor)

# Sequencias

## Sequência de 1 a 10
seq1 <- seq(1, 10)
seq1

## Sequência de 10 a 1, de 2 em 2
seq2 <- seq(10, 1, by = -2)
seq2

## Repete 1, 2 e 3, 3 vezes
rep1 <- rep(c(1, 2, 3), times = 3)
rep1

## Repete 1, 2 e 3, 3 vezes cada
rep2 <- rep(c(1, 2, 3), each = 3)
rep2

# Números Aleatórios

## 5 Números aleatórios entre 0 e 1
runif(5)

## 5 Números aleatórios de uma normal, com média 0 e desvio padrão 1
rnorm(5)

## Amostras aleatórias
sample(numeros, 
       size = 3, 
       replace = FALSE)

sample(letras,
       size = 5, 
       replace = TRUE)

# Operações Estatísticas

y <- c(7, 5, 2, 2, 4, 8, 
       5, 2, 6, 4, 5, 10, 
       3, 2, 6, 10, 7, 8,
       6, 10, 3, 4, 5, 1)

## Número de elementos
length(y)

## Soma dos elementos
sum(y)

## Média
mean(y)

## Mediana
median(y)

## Máximo
max(y)

## Máximo
max(y)

## Variância
var(y)

## Desvio padrão
sd(y)

## Desvio absoluto mediana
mad(y, constant = 1)

## Coeficiente de variação
100 * sd(y) / mean(y)

## Quartis
quantile(y)

## Amplitude Interquartilica
IQR(y)

## Tabela de Frequência
table(y)


# Matrizes

## Cria uma matriz 2x3
matriz <- matrix(1:6, nrow = 2)
matriz

## Cria uma matriz 3x2
matriz2 <- matrix(1:6, ncol = 2)
matriz2


## Criando matriz a partir de um vetor
vet1 <- c(1, 2, 3)
vet2 <- c(4, 5, 6)
### Junta vet1 e vet2 por linha
matriz3 <- rbind(vet1, vet2)
matriz3
### Junta vet1 e vet2 por coluna
matriz4 <- cbind(vet1, vet2)
matriz4 

# Dataframes

df <- data.frame(
  nome = c("João", "Ana", "Carlos"),
  idade = c(25, 30, 22),
  altura = c(1.75, 1.60, 1.80)
) 
df

names(df)

df$nome

dim(df)

str(df)

df$cidade <- c('Curitiba','Londrina','Maringá') 

df['pagou'] = c(T,F,T)

df$peso <- c(80,67,92)

df['IMC'] <- df$peso / (df$altura^2)

### Filtrando Dados

df[df$IMC > 27,]

subset(df, IMC > 27, select = 'nome')

### Sumarizar Dados

summary(df)

### Ordernar dados


df[order(df$idade), ]

df[order(df$idade, decreasing = TRUE), ]

# Data Table

## install.packages('data.table')
library(data.table)

## Criar um data.table
dt <- data.table(
  nome = c("Alice", "Bob", "Carol", "Ana", "João", "Carlos", "Patrícia", "Leonardo"),
  idade = c(25, 30, 28, 20, 27, 50, 60, 45),
  salario = c(5000, 6000, 5500, 8000, 2000, 3500, 10000, 3800 ), 
  meio_de_transporte = c('onibus', 'bicicleta', 'onibus', 'carro', 'carro', 'onibus', 'onibus', 'bicicleta'))
dt

dt[idade > 30]

# Lendo dados 
read.table('~/dados.txt')

read.csv('~/DSBD/R/Queimadas.csv')

dt <- data.table::fread('~/DSBD/R/data/Queimadas.csv')
