library(tidyverse)
filename <- "./stang.csv"

## Load the data
df_stang <- read_csv(filename)
df_stang

df <-
  df_stang %>% 
  pivot_longer(
    cols = E_00:nu_90,
    names_to = c(".value", "angle"),
    names_sep = "_",
    names_transform = list(angle = as.integer)
  ) %>% 
  filter(E > 0)
df

mylm <- lm(df$E ~ df$nu)

mylm$coefficients[1]
mylm$coefficients[2]

