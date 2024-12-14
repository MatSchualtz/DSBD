library(tidyverse)

mtcars <- mtcars

#Utilize o banco de dados mtcars para criar um gráfico de dispersão entre as variáveis mpg e wt. 
#Colora os pontos de acordo com a variável cyl e ajuste o tamanho dos pontos de acordo com a variável hp.

mtcars %>% 
  ggplot() +
  aes(x = mpg, 
      y = wt, 
      color = factor(cyl), 
      size = hp) +
  geom_point() + 
  labs(title = "Relação entre mpg e wt",
       x = "Milhas por galão",
       y = "Peso do carro") +
  theme_minimal()

#Utilize o banco de dados mtcars para criar um gráfico de linhas entre as variáveis mpg e wt. 
#Colora as linhas de acordo com a variável cyl e ajuste o tipo de linha de acordo com a variável cyl.

mtcars %>% 
  ggplot() +
  aes(x = mpg, 
      y = wt, 
      color = factor(cyl), 
      linetype = factor(cyl)) +
  geom_line() + 
  labs(title = "Relação entre mpg e wt",
       x = "Milhas por galão",
       y = "Peso do carro", 
       color = "Cilindros", 
       linetype = "Cilindros") +
  theme_minimal()


#Utilize o banco de dados mtcars para criar um gráfico de barras para visualizar a relação entre a variável cyl e a variável mpg.


mtcars %>% 
  ggplot() +
  aes(x = factor(cyl), 
      y = mpg) +
  geom_bar(stat = "summary", fun = "mean") + 
  labs(title = "Relação entre cyl e mpg",
       x = "Número de cilindros",
       y = "Milhas por galão") +
  theme_minimal()


#Utilize o banco de dados mtcars para criar um histograma para visualizar a distribuição da variável mpg.

mtcars %>% 
  ggplot() +
  aes(x = mpg) +
  geom_histogram(binwidth = 5) + 
  labs(title = "Distribuição de mpg",
       x = "Milhas por galão",
       y = "Frequência") +
  theme_minimal()


#Utilize o banco de dados mtcars para criar um gráfico de densidade para visualizar a distribuição da variável mpg.

mtcars %>% 
  ggplot() +
  aes(x = mpg) +
  geom_density() + 
  labs(title = "Distribuição de mpg",
       x = "Milhas por galão",
       y = "Densidade") +
  theme_minimal()

#Utilize o banco de dados mtcars para criar um boxplot para visualizar a distribuição da variável mpg de acordo com a variável cyl.

mtcars %>% 
  ggplot() +
  aes(x = factor(cyl), 
      y = mpg) +
  geom_boxplot() + 
  labs(title = "Distribuição de mpg por cyl",
       x = "Número de cilindros",
       y = "Milhas por galão") +
  theme_minimal()

#Utilize o banco de dados mtcars para criar um gráfico de violino para visualizar a distribuição da variável mpg de acordo com a variável cyl.

mtcars %>% 
  ggplot() +
  aes(x = factor(cyl), 
      y = mpg) +
  geom_violin() + 
  labs(title = "Distribuição de mpg por cyl",
       x = "Número de cilindros",
       y = "Milhas por galão") +
  theme_minimal()




