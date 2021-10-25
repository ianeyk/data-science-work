library(tidyverse)
library(ggmap)
library(tools)
load("C:/dev/git/data-science/data-science-work/challenges/covid1/df_covid.RData")

map <- map_data("county", region = "Oregon") %>% 
  mutate(county = tolower(subregion), .keep = "unused") %>% 
  filter(county == "coos")

df2 <- 
  df_covid %>% 
  filter(state == "Oregon", date == max(date), county == "Coos") %>% 
  mutate(county = tolower(county), cases = as.numeric(cases)) %>%
  select(county, cases)

ggplot(df2) + 
  geom_map(aes(map_id = county, fill = cases), map = map) +
  expand_limits(x = map$long, y = map$lat)

us <- map_data("state")

USArrests2 <- USArrests %>% 
  rownames_to_column("region") %>% 
  mutate(region = tolower(region))

ggplot(USArrests2) +
  geom_map(aes(map_id = region, fill = UrbanPop), map = us) +
  expand_limits(x = us$long, y = us$lat)

us <- map_data("state")
ggplot(us) + 
  geom_map(aes(map_id = region), map = us, fill = "transparent", color = "black") + 
  expand_limits(x = us$long, y = us$lat)

