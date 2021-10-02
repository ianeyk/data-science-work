library(tidyverse)

mpg %>%
  ggplot(mapping = aes(x = class, y = hwy, color = class)) +
  geom_boxplot() +
  labs(
    x = "class", 
    y = "highway efficiency (mgp)",
    title = "Efficiency of different vehicle types"
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1)) +
  scale_color_brewer(palette = "Set1")
