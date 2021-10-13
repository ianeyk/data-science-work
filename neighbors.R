library(tidyverse)
load("covid2/covid.RData")

# initialize df_covid
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

# process one State's data into new cases per day, 
# summarizing by state, with a lag of nDiff = 1
processState <- function(state1, nDiff) {
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

# computes and returns the correlation between
# two States' covid trends over time 
# (neglecting the date of observations)
corrNeighbor <- function(state1, state2) {
  return <- 
    processState(state1, 1) %>% 
    inner_join(
      processState(state2, 1), 
      by = c("date" = "date")
    ) %>% 
    select(!date) %>% 
    cor(use = "complete.obs") %>%
    .[2, 1] # select the 2, 1th element (the r value)
}

# loops through every pair of neighboring states 
# and calculates the correlation between them.
corrNeighbors <- function(state, borders) {
  for (neighbor in borders) {
    print(paste(state, neighbor, sep = "+"))
  }
}

# loops through every state, finds neighboring states, 
# and calculates the correlation between them.
for (row in 1:nrow(neighbors)) {
  state <- neighbors[row, "state"]
  borders <- neighbors[row, "borders"]
  corrNeighbors(state, borders)
}

