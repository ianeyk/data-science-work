stateCorrs %>%
  ggplot(mapping = aes(x = state, y = corr)) +
  geom_text(aes(label = neighbor), size = 2) + 
  labs(
    x = "State", 
    y = "corr"
  ) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1))