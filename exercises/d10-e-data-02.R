## d10-e-data-02 code chunks

library(tidyverse)
library(babynames)

babynames %>% 
  filter(name == "Ian") %>% 
  summarise(total = sum(n), max = max(n), mean = mean(n))

babynames %>% 
  filter(name == "Khaleesi") %>% 
  summarise(earliest = first(year))

babynames %>% 
  summarise(n = n(), distinct = n_distinct(name))

babynames %>% 
  group_by(year, sex) %>% 
  summarise(total = sum(n)) %>% 
  summarise(total = sum(total))

babynames %>% 
  group_by(name, sex) %>% 
  summarize(total = sum(n)) %>% 
  arrange(desc(total))

library(digest)
top_10_names <- babynames %>% 
  group_by(name, sex) %>% 
  summarize(total = sum(n)) %>% 
  arrange(desc(total)) %>% 
  ungroup() %>% 
  slice(1:10)

# best solution
top_10 <- babynames %>% 
  semi_join(top_10_names)

top_10 %>% 
  ggplot(mapping = aes(x = year, y = prop, color = name)) +
  geom_line()

babynames %>% 
  filter(name == "James", sex == "M") %>% 
  summarize(n = sum(n))

babynames %>% 
  mutate(births = n / prop)

babynames %>% 
  group_by(year, sex) %>% 
  mutate(rank = min_rank(desc(prop))) %>% 
  group_by(name, sex) %>% 
  summarize(median = median(rank)) %>% 
  arrange(median)


babynames %>% 
  group_by(year, sex) %>% 
  mutate(rank = min_rank(desc(prop))) %>% 
  filter(rank == 1, sex == "M") %>% 
  ungroup() %>% 
  distinct(name) %>%
  summarize(num = n())

number_ones <- 
  babynames %>% 
  group_by(year, sex) %>% 
  mutate(rank = min_rank(desc(prop))) %>% 
  filter(rank == 1, sex == "M") %>% 
  ungroup() %>% 
  distinct(name) %>%
  unlist() %>%
  print()

babynames %>% 
  filter(name %in% number_ones, sex == "M") %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = prop, color = name))

babynames %>% 
  group_by(year, sex) %>% 
  summarize(n_names = n()) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = n_names, color = sex))

babynames %>% 
  group_by(year, sex) %>% 
  summarize(total = sum(n)) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = total, color = sex))

babynames %>% 
  group_by(year, sex) %>% 
  summarize(children_per_name = sum(n) / n()) %>% 
  ggplot() + 
  geom_line(mapping = aes(x = year, y = children_per_name, color = sex))

