library(tidyverse)
library(babynames)

most_popular_1880 <- babynames %>%
    filter(year == 1880) %>% 
    arrange(desc(prop)) %>% 
    slice_head(n = 4) %>%
    print()

all_others <- babynames %>% 
    mutate(name = ifelse(
        name %in% unlist(most_popular_1880), 
        name, 
        "other"))

ggplot(all_others) +
    geom_area(mapping = aes(x = year, y = n, fill = name))

ggplot(newdf) +
    # scale_y_continuous(trans='log10') +
    geom_area(mapping = aes(x = year, y = total, fill = name))


