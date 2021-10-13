library(tidyverse)
load("covid2/covid.RData")

df_covid <- 
  df_covid %>% 
  mutate(
    state = state %>% tolower, 
    county = county %>% tolower
  )

neighbors <- 
  read.csv("covid2/NeighboringStates.csv") %>% 
  mutate(
    state = State.Name %>% tolower(), 
    borders = Bordering.States %>% 
      tolower() %>% 
      str_split(", "), 
    .keep = "none"
  )

extractState <- function(state1, nDiff) {
  return <- 
    df_covid %>% 
    filter(state == state1) %>% 
    group_by(date, state) %>% 
    mutate(cases = sum(cases)) %>% 
    ungroup() %>% 
    distinct(date, state, .keep_all = TRUE) %>% 
    select(date, cases) %>% 
    mutate(
      cases = c(NA, diff(cases, lag = nDiff))
    )
}

corrNeighbor <- function(state1, state2) {
  return <- 
    extractState(state1, 1) %>% 
    inner_join(
      extractState(state2, 1), 
      by = c("date" = "date")
    ) %>% 
    select(!date) %>% 
    cor(use = "complete.obs") %>%
    .[2, 1] # select the 2, 1th element (the r value)
}

corrNeighbors <- function(state, borders) {
  for (neighbor in borders) {
    print(paste(state, neighbor, sep = "+"))
  }
}

for (row in 1:nrow(neighbors)) {
  state <- neighbors[row, "state"]
  borders <- neighbors[row, "borders"]
  corrNeighbors(state, borders)
}

