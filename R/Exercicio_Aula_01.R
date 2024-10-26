# Utilize o banco de dados: Queimadas.csv e faça o que se pede:

## 1. Carregue o banco de dados.

dt <- data.table::fread('~/DSBD/R/data/Queimadas.csv')

## 2. Verifique as primeiras linhas do banco de dados.

head(dt)

## 3. Verifique as últimas linhas do banco de dados.

tail(dt)

## 4. Verifique o sumário das informações.

summary(dt)

## 5. Verifique a estrutura do banco de dados.

str(dt)

## 6. Verifique o nome das colunas.

names(dt)

## 7. Verifique a quantidade de linhas e colunas.

dim(dt)

## 8. Verifique a quantidade de valores ausentes na coluna bioma e na coluna avg_frp.

sum(is.na(dt$avg_frp))

## 9. Quantas observações existem no bioma Caatinga com avg_numero_dias_sem_chuva menor do que 30?

dt[bioma == 'Caatinga' & avg_numero_dias_sem_chuva < 30, .N]

## 10. Quantas observações existem no bioma Cerrado com avg_numero_dias_sem_chuva maior do que 50 ou avg_precipitacao maior do que 10?

dt[bioma == 'Cerrado' & avg_numero_dias_sem_chuva > 30 & avg_precipitacao > 10, .N]


## 11. Qual a média de avg_frp para o bioma Mata Atlântica?

dt[bioma == "Mata Atlântica", .(media_avg_frp = mean(avg_frp, na.rm = T))]
