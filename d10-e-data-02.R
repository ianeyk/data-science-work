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
  slice(1:10) %>% 

top_10 <- babynames %>% 
  # filter((name == "Ian" & sex == "M") | (name == "Sam" & sex == "F"))
View(top_10)

# best solution
top_10 <- babynames %>% 
  semi_join(top_10_names)

top_10 %>% 
  ggplot(mapping = aes(x = year, y = prop, color = name)) +
  geom_line()

babynames %>% 
  filter(name == "James", sex == "M") %>% 
  summarize(n = sum(n))



