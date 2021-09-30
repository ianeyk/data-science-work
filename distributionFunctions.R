library(tidyverse)

normvec <- tibble(xvec = -50:50 / 10, zvec = pnorm(xvec))
normvec %>%
  ggplot(mapping = aes(x = xvec, y = zvec)) +
  geom_point()

labs(
  x = "x_label", 
  y = "y_label",
  title = "title"
) + 
theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))
