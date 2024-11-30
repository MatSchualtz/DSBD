#install.packages("tidyverse")
library(tidyverse)

dados <- readr::read_csv("/home/espinf/msschualtz/DSBD/R/data/Mental Health Dataset.csv")

## Para vermos o resumo dos dados, podemos utilizar a função glimpse()
glimpse(dados)

## Criando variáveis com mutate

dados <- dados %>% 
  mutate(mercosul = ifelse(Country %in%
                             c("Argentina", "Brazil", "Paraguay", "Uruguay"),
                           "Mercosul", "Não Mercosul"))
glimpse(dados)

## Selecionando variáveis com select 

### 1º Método: Declarar a seleção

dados2 <- dados %>% 
  select(Country, Timestamp, Days_Indoors, mercosul)
glimpse(dados2)

### 2º Método: Seleção por índices

dados3 <- dados %>% 
  select(3:5)
glimpse(dados3)

### 3º Método: Seleção Intervalar

dados4 <- dados %>% 
  select(treatment:Changes_Habits)
glimpse(dados4)

### OBS: Podemos utilizar a função starts_with(), ends_with(), contains() e matches() para selecionar variáveis que atendam a um padrão. 
### E caso a ideia seja selecionar por tipo, é possível usar select(where= is.type) ou select_if(is.type)

### Para remover variáveis, podemos utilizar a função select() com o operador -.

dados5 <- dados %>% 
  select(-Country, -Timestamp, -Days_Indoors, -mercosul)
glimpse(dados5)


## Filtrando Observações com filter()

dados %>% filter(Country == 'Brazil')

dados %>% filter(Country %in% c('Brazil',
                                'Argentina',
                                'Uruguay'))

## Ordenando Observações com arrange()

dados %>% arrange(desc(Country)) %>% View()

#Agrupando Dados com group_by()


require(magrittr)
dados %<>%
  mutate(numeric = runif(nrow(dados)))

dados %>% group_by(Country) %>%
  summarise(media = mean(numeric),
            n = n())



# Trabalhando com Datas

car_crash = read_csv("data/Brazil Total highway crashes 2010 - 2023.csv")

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(ano = year(data),
         mes = month(data),
         dia = day(data)) %>%
  select(data, ano, mes, dia) %>%
  head()

## Podemos calcular a diferença entre duas datas utilizando a função difftime().

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(dias_desde_acidente = difftime(Sys.Date(), data, units = "days")) %>%
  select(data, dias_desde_acidente) %>%
  head()


## Podemos somar ou subtrair dias de uma data utilizando a função lubridate::days().

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(data_mais_10_dias = data + lubridate::days(10)) %>%
  select(data, data_mais_10_dias) %>%
  head()


## Podemos extrair a hora, os minutos e os segundos de uma data utilizando as funções hour(), minute() e second().

data <- ymd_hms("2023-08-21 15:30:45")
ano <- year(data); ano
mes <- month(data); mes
dia <- day(data); dia
hora <- hour(data); hora 
minuto <- minute(data); minuto 
segundo <- second(data); segundo

## Podemos converter o fuso horário de uma data utilizando a função with_tz().

### Data original no fuso horário de Nova Iorque
data_ny <- ymd_hms("2023-08-21 12:00:00", tz = "America/New_York")

### Converter para o fuso horário de Londres
data_london <- with_tz(data_ny, tz = "Europe/London")

print(data_ny)
