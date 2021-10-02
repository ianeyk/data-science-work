library(tidyverse)
library(ggrepel)
library(gginnards)

## NOTE: If you extracted all challenges to the same location,
## you shouldn't have to change this filename
filename <- 
  "C:/dev/git/data-science/ianeyk-ds-f2021/challenges/data/antibiotics.csv"

## Load the data
df_antibiotics <- read_csv(filename)
df_antibiotics %>% knitr::kable()

df_antibiotics_2 <- df_antibiotics %>% 
  pivot_longer(
    cols = ends_with("in"),
    names_to = "antibiotic",
    values_to = "MIC"
  )

df_antibiotics_3 <- 
  df_antibiotics_2 %>% 
  mutate(
    id_num = group_indices(df_antibiotics_2, bacteria)
  )

  
sequence_length <- length(unique(df_antibiotics_2$bacteria))
first_sequence  <- c(1:(sequence_length%/%2))
second_sequence <- c((sequence_length%/%2+1):sequence_length) 
first_angles    <- c( 90 - 180/length(first_sequence) * first_sequence)
second_angles   <- c(-90 - 180/length(second_sequence) * second_sequence)
polar_angles    <- c(first_angles, second_angles) + 180 / sequence_length

p1 <- p0 + 
  df_antibiotics_2 %>%
  arrange(gram) %>% 
  mutate(
    max_effective_MIC = MIC * 10,
    id_num = group_indices(df_antibiotics_2, bacteria),
    bacteria = fct_reorder(bacteria, gram)
  ) %>%
  ggplot(
    mapping = aes(
      x = bacteria, 
      y = max_effective_MIC, 
      fill = antibiotic
    )
  ) + 
  coord_polar() +
  geom_col(position = "dodge") + 
  scale_y_log10() + 
  labs(
    x = "Bacterium",
    y = "Multiples of the maximum effective MIC",
    title = "Antibiotic Resistance"
  ) +
  theme(
    axis.text.x = element_text(
      vjust = 0.25,
      hjust = 1,
      size = 7,
      angle = polar_angles
    )
  )
p1

num_negative <- 
  df_antibiotics_2 %>% 
  filter(gram == "negative") %>% 
  count(gram) %>% 
  pull(n)

num_positive <- 
  df_antibiotics_2 %>% 
  filter(gram == "positive") %>% 
  count(gram) %>% 
  pull(n) 

p2 <- p1 + 
  geom_rect(
    xmin = 0.5, 
    xmax = num_negative / 3 + 0.5, 
    ymin = 0, 
    ymax = 5, 
    fill = "coral2"
  ) + 
  geom_rect(
    xmin = num_negative / 3 + 0.5, 
    xmax = 48.5, 
    ymin = 0, 
    ymax = 5, 
    fill = "cadetblue3") + 
  scale_fill_manual(values = c("blue1", "brown4", "black")) + 
  theme(panel.background = element_rect(fill = "bisque2"))
move_layers(p2, "GeomRect", position = "bottom")

ggsave(
  file = "C:/dev/git/data-science/data-science-work/test.svg", 
  width = 15, 
  height = 10
)  

