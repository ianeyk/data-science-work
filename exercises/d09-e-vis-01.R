## d09-e-vis-01 reading exercises
## https://rstudio.cloud/learn/primers/3.2

library(tidyverse)

diamonds %>% 
    count(color) %>% 
    ggplot() +
    geom_col(mapping = aes(x = color, y = n))

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = cut))

mpg %>% 
    ggplot() +
    geom_bar(mapping = aes(x = class, fill = class))

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity))

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")

diamonds %>% 
    ggplot() + 
    geom_bar(mapping = aes(x = color, fill = clarity), position = "fill", width = 1.0)

diamonds %>% 
    ggplot() + 
    geom_bar(mapping = aes(x = color, fill = cut), position = "dodge")

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = color)) +
    facet_grid(clarity ~ cut)

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = color)) +
    facet_grid(. ~ cut)

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = color)) +
    facet_grid(clarity ~ .)

ggplot(data = diamonds) +
    geom_bar(mapping = aes(x = color, fill = cut), position = "stack") +
    facet_wrap( ~ clarity)

