library(tidyverse)

df_who <- who %>% 
  select(!c("iso2", "iso3")) %>% 
  pivot_longer(
    cols = starts_with("new"),
    names_to = "parameters",
    values_to = "cases"
  ) %>% 
  separate(
    col = parameters,
    into = c("new", "TB", "the_rest"),
    sep = "_",
    remove = TRUE
  ) %>% 
  separate(
    col = the_rest,
    into = c("sex", "age_range"),
    sep = 1,
    remove = TRUE
  ) %>% 
  select(!new) %>% 
  drop_na(cases)

