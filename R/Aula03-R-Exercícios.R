library(tidyverse)


#1.Utilizando o banco de dados storms, faça o que se pede:
data(storms)

#Filtre as observações cujo tipo de evento é Tropical Depression. Quantas observações existem?
storms %>% 
  filter(event == "Tropical Depression") %>% 
  nrow()

#Filtre as observações cujo tipo de evento é Tropical Depression e a velocidade do vento é maior ou igual a 40. Quantas observações existem?

storms %>%
  filter(event == "Tropical Depression" & wind >= 40) %>%
  nrow()

#Selecione as variáveis numéricas e ordene as observações pela variável pressure em ordem crescente.

storms %>%
  select_if(is.numeric) %>%
  arrange(pressure)



#2.Utilizando o banco de dados starwars faça o que se pede:
data("starwars")

# Qual é o número total de espécies únicas presentes? Qual a frequência de indivíduos por espécie?
starwars %>%
  summarise(n_especies = n_distinct(species))

starwars %>%
  group_by(species) %>%
  summarise(freq_especies = n()) %>%
  arrange(desc(freq_especies))
  

# Calcule a altura média de personagens masculinos e femininos.
starwars %>% 
  filter(sex %in% c("female", "male")) %>% 
  group_by(sex) %>%
  summarise(media_altura = mean(height, na.rm = TRUE))

# Qual é a média de peso dos personagens de cada espécie para personagens masculinos?
starwars %>% 
  filter(sex == "male") %>%
  group_by(species) %>%
  summarise(media_peso = mean(mass, na.rm = TRUE))


# Para cada espécie presente na base de dados, identifique o personagem mais pesado e seu peso correspondente.

starwars %>% 
  group_by(species) %>%
  filter(mass == max(mass, na.rm = TRUE)) %>%
  select(species, name, mass)


# 3. Utilizando o banco de dados car_crash faça o que se pede:

car_crash = read_csv("data/Brazil Total highway crashes 2010 - 2023.csv")

# Quais os meses do ano com maior número de acidentes fatais?

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(mes = month(data)) %>%
  select(data, mes, mortos) %>%
  filter(mortos > 0) %>% 
  group_by(mes) %>%
  summarise(total = n()) %>%
  arrange(desc(total))
  
  

# Quais os dias da semana com maior número de acidentes fatais?
# Dica: Busque por uma função que retorne o dia da semana a partir de uma data.

car_crash %>% 
  mutate(data = dmy(data)) %>%
  mutate(dia_semana = lubridate::wday(data, label = T, abbr = F)) %>%
  select(dia_semana, mortos) %>%
  filter(mortos > 0) %>% 
  group_by(dia_semana) %>%
  summarise(total_mortos_dia = sum(mortos)) %>% 
  arrange(desc(total_mortos_dia))



#4.Utilize os dados do pacote nycflights13 para responder as perguntas abaixo:
#Para vôos com atraso superior a 12 horas em flights, verifique as condições climáticas em weather. 
#Quais os meses do ano em que você encontra o maior número de atrasos?
require(nycflights13)
flights %>% 
  filter(arr_delay > 12*60 | dep_delay > 12*60) %>% 
  inner_join(weather, by = c("year", "month", "day", "hour", "origin")) %>% 
  group_by(month) %>% 
  summarise(total_atrasos = n(),
            mean_temp = mean(temp, na.rm = T),
            mean_dewp = mean(dewp, na.rm = T),
            mean_humid = mean(humid, na.rm = T),
            mean_wind_speed = mean(wind_speed, na.rm = T),
            mean_precip = mean(precip, na.rm = T)
  ) %>% 
  arrange(desc(total_atrasos))

#Encontre os 20 destinos mais comuns e identifique seu aeroporto.
flights %>% 
  group_by(dest) %>% 
  summarise(total_voos = n()) %>% 
  arrange(desc(total_voos)) %>% 
  head(20) %>% 
  inner_join(airports, by = c("dest" = "faa"))



