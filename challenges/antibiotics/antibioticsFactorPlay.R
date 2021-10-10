df_antibiotics_2 %>%
  separate(
    col = bacteria,
    into = c("genus", "species"),
    remove = FALSE,
    sep = " "
  ) %>%
  group_by(genus) %>%
  filter(MIC <= 0.1) %>%
  ggplot(mapping = aes(x = antibiotic, y = MIC, color = genus, shape = gram)) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) +
  labs(title = 'Drug Treatment Based on Genus')

df_genus_d <- 
  df_antibiotics_2 %>%
  separate(
    col = bacteria,
    into = c("genus", "species"),
    remove = FALSE,
    sep = " "
  )

df_genus_d$genus <- 
  replace(
    df_genus_d$genus, 
    !(
      df_genus_d$genus %in% c(
        "Streptococcus", "Staphylococcus", "Salmonella"
      )
    ), 
    "Other"
  )

df_genus_d %>% 
  ggplot(mapping = aes(x = 0, y = MIC, color = genus)) +
  # geom_point() + 
  geom_jitter(height = 0, width = .3) +
  # labs(title = 'Drug Treatment Based on Genus') + 
  facet_grid(gram ~ antibiotic) + 
  scale_y_log10() + 
  geom_hline(yintercept = 0.1, linetype = "dashed") + 
  theme(
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.4, "cm"), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )
