library(ggpattern)

p <- ggplot(mpg, aes(class)) +
  geom_bar_pattern(
    aes(
      pattern_angle = class
    ),
    pattern         = 'placeholder',
    pattern_type    = 'kitten',
    fill            = 'white',
    colour          = 'black',
    pattern_spacing = 0.025
  ) +
  theme_bw(18) +
  labs(
    title = "ggpattern::geom_bar_pattern()",
    subtitle = "pattern = 'placeholder', pattern_type = 'kitten'"
  ) +
  theme(legend.position = 'none') +
  coord_polar() + 
  # coord_fixed(ratio = 1/15) +
  scale_pattern_discrete(guide = guide_legend(nrow = 1))
p



mpg %>%
  ggplot() +
  geom_density(
    mapping = aes(x = displ, y = hwy, color = class)
  )

mpg %>%
  ggplot() +
  geom_density(
    mapping = aes(x = displ, y = hwy, color = class))

mpg %>%
  ggplot() +
  geom_density(mapping = aes(x = displ, y = hwy), stat = "identity")

