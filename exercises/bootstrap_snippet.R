df_q2 <- tibble(y = rnorm(50))
# 
# Calculate summaries of each sub-sample
my_bootstraps <-
  bootstraps(df_q2, times = 2000) %>% 
  mutate(
    df_q2 = map(splits, analysis), 
    estimates = map(
      splits,
      function(df_q2) {
        analysis(df_q2) %>% 
        pull(y) %>%
        fitdistr(densfun = "lognormal") %>%
        tidy()
      }
    )
  )
# 
# Display confidence intervals
int_pctl(my_bootstraps, estimates, alpha = 0.05)

