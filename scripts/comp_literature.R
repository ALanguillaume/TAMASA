sampled_data_long <- sampled_data %>% 
  select(matches("tlab_[a-z]_ha_[a-z]{3}")) %>%
  pivot_longer(cols = matches("tlab_[a-z]_ha_[a-z]{3}"),
               names_to = c(".value", "cropsys"),
               names_pattern = "(tlab_[a-z]_ha)_([a-z]{3})$")

dlab <- sampled_data_long %>%
  split(foo$cropsys) %>%
  map(~ select(.x, -cropsys)) %>%
  map(rowSums) %>%
  map(~ data.frame(days_ha = .x)) %>%
  bind_rows(.id = "cropsys") %>%
  mutate(hours_ha_4 = days_ha * 4,
         hours_ha_6 = days_ha * 6,
         hours_ha_8 = days_ha * 8) %>%
  pivot_longer(cols = starts_with("hours_ha"),
               names_to = c(".value", "nb_hours_per_day"),
               names_pattern = "(.*)_(\\d)$")


ggplot(dlab)+
  aes(y = days_ha, x = 1)+
  geom_violin(alpha = 0.3)+
  ggbeeswarm::geom_quasirandom(alpha = 0.5)+
  # ylim(0, 1000)+
  facet_grid(cropsys ~ nb_hours_per_day)+
  xlab("")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
