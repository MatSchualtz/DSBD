library(tidyverse)

#Visualização de Dados com ggplot2

## Dispersão

iris %>% 
  ggplot() +
  aes(x = Petal.Length, y = Petal.Width) + 
  geom_point()


iris %>% 
  ggplot() +
  aes(x = Petal.Length, 
      y = Petal.Width, 
      color = Species, 
      size = Sepal.Length) + 
  geom_point(alpha = 0.4, pch = "circle")


## Linhas 

iris %>%
  ggplot() +
  aes(x = Sepal.Length, y = Sepal.Width) +
  geom_line()

iris %>% 
  ggplot() +
  aes(x = Sepal.Length, 
      y = Sepal.Width, 
      color = Species, 
      linetype = Species) + 
  geom_line()


## Combinando Geometrias

iris %>% 
  ggplot() +
  aes(x = Sepal.Length, 
      y = Sepal.Width, 
      color = Species, 
      linetype = Species) + 
  geom_line() +
  geom_point()


## Barras

iris %>% 
  count(Species) %>% 
  ggplot() +
  aes(x = Species, 
      y = n, 
      color = Species, 
      fill = Species) +
  geom_bar(stat = "identity")


## Histograma

iris %>% 
  ggplot() +
  aes(x = Sepal.Length) +
  geom_histogram(color = "red", 
                 fill = "salmon", 
                 bins = 30)

## Densidade 

iris %>% 
  ggplot() +
  aes(x = Sepal.Length, 
      color = Species, 
      fill = Species) +
  geom_density(alpha = 0.5)

## Boxplot

iris %>% 
  ggplot() +
  aes(x = Species, y = Sepal.Length, fill = Species) +
  geom_boxplot()

## Violino 

iris %>%
  ggplot() +
  aes(x = Species, y = Sepal.Length, fill = Species) +
  geom_violin() + 
  geom_boxplot(width = 0.1, fill = "white")