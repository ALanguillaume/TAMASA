


library(tidyverse)
library(here)


source("./scripts/plot_funcs.R")
source("./scripts/TAMASA_lhc.R")


vars_df <- TAMASA_prepare_lhc() 

sampled_data <- TAMASA_sample_lhc(vars_df, n = 100)

TAMASA_plot_lhc(vars_df, sampled_data)


##### Load data 

# load all extracted data
lf <- list.files("./data/extracted/")
# List with all data files...
ld <- map(here("./data/extracted/", lf), ~ read.csv(.x))
names(ld) <- str_remove(lf, "\\.csv")
# keep only data for the Northern Zone
ld_N <- map(ld, ~ .x[.x$zone == "N", ])
# ...then split into separate objects
list2env(ld_N, envir = parent.frame())


household_lab <- household %>%
  select(hhid, plot_id, crop, cropsys, matches("tlab_"))

# Data from literature

d_leonardo15 <- data.frame(hours_ha = c(750, 990, 980, 1270, 930, 880, 870, 1140))

mean_x = 92.8
sd_x = 73.1
sc = sd_x^2 / mean_x
sh = mean_x^2 / sd_x^2 
sim_d_silva18 <- data.frame(silva_dha = rgamma(100, shape = sh, scale = sc))

# cbind(sampled_data, sim_d_silva18)

d_montt19 <- readxl::read_xlsx("./data/data_literature_financial return.xlsx", sheet = "Montt2019")
names(d_montt19) <- names(d_montt19) %>%
  str_remove(" \\(person days/ha\\)") %>%
  str_replace(" ", "_") %>% 
  tolower()

# raw TAMASA data

TAMASA_tlab_raw <- household_lab %>%
  select(cropsys, matches("tlab_[a-z]_ha"))

TAMASA_tlab_raw$id <- 1:nrow(TAMASA_tlab_raw)

TAMASA_tlab_raw <- TAMASA_tlab_raw %>% 
  split(TAMASA_tlab_raw $id) %>%
  discard(~ any(is.na(.x))) %>%
  bind_rows() %>%
  mutate(days_ha = rowSums(select(., matches("tlab_[a-z]_ha")))) %>%
  mutate(hours_ha_4 = days_ha * 4,
         hours_ha_6 = days_ha * 6,
         hours_ha_8 = days_ha * 8) %>%
  pivot_longer(cols = starts_with("hours_ha"),
               names_to = c(".value", "nb_hours_per_day"),
               names_pattern = "(.*)_(\\d)$")


# Process lhc labour data

sampled_data_long <- sampled_data %>% 
  select(matches("tlab_[a-z]_ha_[a-z]{3}")) %>%
  pivot_longer(cols = matches("tlab_[a-z]_ha_[a-z]{3}"),
               names_to = c(".value", "cropsys"),
               names_pattern = "(tlab_[a-z]_ha)_([a-z]{3})$")

lhc_lab <- sampled_data_long %>%
  split(sampled_data_long$cropsys) %>%
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


d_lhc_and_raw <- list(lhc = lhc_lab, 
                      raw =  select(TAMASA_tlab_raw,
                                    cropsys, days_ha, nb_hours_per_day, hours_ha)) %>%
  bind_rows(.id = "source")

# windows()
ggplot(lhc_lab)+
  aes(y = hours_ha, x = 1)+
  geom_violin(alpha = 0.3)+
  ggbeeswarm::geom_quasirandom(alpha = 0.5)+
  ggbeeswarm::geom_quasirandom(data = d_leonardo15, aes(y = hours_ha), color = "red", alpha = 0.8)+
  # ylim(0, 1000)+
  facet_grid(cropsys ~ nb_hours_per_day)+
  xlab("")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# windows()
ggplot(d_lhc_and_raw)+
  aes(y = hours_ha, x = source, colour = source)+
  geom_violin(alpha = 0.3)+
  ggbeeswarm::geom_quasirandom(alpha = 0.5)+
  ggbeeswarm::geom_quasirandom(data = d_leonardo15, aes(y = hours_ha, x = 1), color = "red", alpha = 0.8)+
  # ylim(0, 1000)+
  facet_grid(cropsys ~ nb_hours_per_day)+
  xlab("")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# windows()
ggplot(d_lhc_and_raw)+
  aes(y = days_ha, x = source, color = source)+
  geom_violin(alpha = 0.3)+
  ggbeeswarm::geom_quasirandom(alpha = 0.5)+
  # ggbeeswarm::geom_quasirandom(data = d_leonardo_15, aes(y = hours_ha), color = "red", alpha = 0.8)+
  # ylim(0, 1000)+
  facet_grid(cropsys ~ .)+
  xlab("")+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# windows()
plot_var(sim_d_silva18, silva_dha, plot.unit = "days/ha")

d_montt19 %>%
  pivot_longer(cols = everything(),
               names_to = "var",
               values_to = "days_ha") %>%
  filter(var != "total_labour") %>%
  ggplot()+
  aes(y = days_ha, x = 1)+
  geom_violin(alpha = 0.3)+
  ggbeeswarm::geom_quasirandom(alpha = 0.5)+
  facet_wrap(var ~ .)

d_montt19 %>%
  pivot_longer(cols = everything(),
               names_to = "var",
               values_to = "days_ha") %>%
  filter(var == "total_labour") %>%
  ggplot()+
  aes(y = days_ha, x = 1)+
  geom_violin(alpha = 0.3)+
  ggbeeswarm::geom_quasirandom(alpha = 0.5)+
  facet_wrap(var ~ .)


