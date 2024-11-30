# Estruturas de Programação

## IF - Else

nota <- 7

if(nota >= 7) {
  print('Aprovado')
} else {
  print('Reprovado')
}


horario <-'manhã'
valor <-  4

if(horario == 'manhã') {
  cat('Bom dia!')
} else if(horario == 'tarde'){
  cat('Boa Tarde!')
} else {
  cat('Boa Noite!')
} 

if(valor <= 100){
  cat('Você ganhou um voucher de 5% na sua próxima compra')
} else {
  cat('Você ganhou um voucher de 10% na sua próxima compra')
  
}  

## Exercício 


#1 - Para quaisquer ponto (x,y) em um plano cartesiano, indique a qual quadrante esse ponto pertence.

ponto <- c(-1,2)

dplyr::case_when(ponto,
                 (ponto[1] > 0) & (ponto[2] > 0) ~ '1º Quadrante',
                 (ponto[1] > 0) & (ponto[2] < 0) ~ '4º Quadrante',
                 (ponto[1] < 0) & (ponto[2] > 0) ~ '2º Quadrante',
                 (ponto[1] < 0) & (ponto[2] < 0) ~ '3º Quadrante'
)

# 2 - Crie um código que, dado um número, indique se ele é par ou ímpar.

numero <- 4

if((numero %% 2) == 0) {
  print('Par')
} else {
  print('Ímpar')
}



## For

### 3 - Utilizando o for loop, calcule os n primeiros números da sequência de Fibonacci. 
        #A sequência de Fibonacci começa com 1, e os números subsequentes são a soma dos dois anteriores (1,1,2,3,5,8,...).

# Número de elementos
n = 10
# Vetor para armazenar os números
Fibonacci <- numeric(n)
# Inicialização dos dois primeiros números
Fibonacci[1] <- 1
Fibonacci[2] <- 1
# Cálculo dos números
for (i in 3:n) {
  Fibonacci[i] <- Fibonacci[i - 1] + Fibonacci[i - 2]
}
Fibonacci


# Inicialização dos dois primeiros números

Fibonacci <- c(1, 1)
i <- 3
while (Fibonacci[i - 1] + Fibonacci[i - 2] <= 10000) {
  Fibonacci[i] <- Fibonacci[i - 1] + Fibonacci[i - 2]
  i <- i + 1
}

Fibonacci

