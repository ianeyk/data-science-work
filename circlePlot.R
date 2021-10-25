load("C:/dev/git/covid-project/spatial_correlation/geo_states.RData")

geo_states %>%
  ggplot(mapping = aes(x = x, y = y, size = state, color = state)) +
  geom_point(show.legend = FALSE) + 
  scale_size_discrete(range = c(1,20))

us <- map_data("state")
# us %>% 
#   left_join(df_2, by = c("region" = "state"))
ggplot(us) + 
  geom_map(
    data = us, 
    mapping = aes(
      map_id = region
    ), 
    map = us, 
    fill = "transparent", 
    color = "black"
    ) + 
    expand_limits(x = us$long, y = us$lat)

