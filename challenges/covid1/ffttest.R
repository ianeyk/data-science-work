ff <-  df_covid %>% 
  mutate(fft = fft(cases), inverse = TRUE)

xs <- seq(-100, 100, 1000)

ff %>%
  ggplot(mapping = aes(x = date, y = Re(fft))) +
  geom_point()

