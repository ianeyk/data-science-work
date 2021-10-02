library(tidyverse)
library(gganimate)
library(gapminder)
library(RColorBrewer)
theme_set(theme_bw())
head(gapminder)

p <- 
  ggplot(
  gapminder, 
  aes(x = gdpPercap, y=lifeExp, size = pop, colour = continent)
  ) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_brewer(palette="Set1") +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  labs(x = "GDP per capita", y = "Life expectancy")

p <- 
    ggplot(
        gapminder, 
        aes(x = gdpPercap, y=lifeExp, size = pop, colour = continent)
    ) +
    geom_point(show.legend = FALSE, alpha = 0.7) +
    scale_color_brewer(palette="Set1") +
    scale_size(range = c(2, 12)) +
    scale_x_log10() +
    labs(x = "GDP per capita", y = "Life expectancy")

p <- p + 
  transition_time(year) +
  labs(title = "Year: {frame_time}")
  # shadow_wake(wake_length = 0.1, alpha = .25)

animate(p, duration = 15)

