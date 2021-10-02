library(tidyverse)
library(ggrepel)

mpg %>%
  ggplot(mapping = aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) + 
  geom_smooth(se = FALSE)

mpg2 <- filter(mpg, class == "2seater")

mpg %>%
  ggplot(mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_point(data = mpg2, color = "red")

mpg40 <- filter(mpg, hwy >= 40)

mpg %>%
  ggplot(mapping = aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_label_repel(data = mpg40, mapping = aes(label = class))

faithful %>%
  ggplot(mapping = aes(x = waiting, y = eruptions)) +
  geom_point() + 
  geom_rug()

