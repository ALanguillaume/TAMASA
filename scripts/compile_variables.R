
library(tidyverse)
library(here)

source("./scripts/funcs.R")
source("./scripts/plot_funcs.R")


##### Load data -----------------------------------------------------------------------------------

# load all extracted data
lf <- list.files("./data/extracted/")
# List with all data files...
ld <- map(here("./data/extracted/", lf), ~ read.csv(.x))
names(ld) <- str_remove(lf, "\\.csv")
# keep only data for the Northern Zone
ld_N <- map(ld, ~ .x[.x$zone == "N", ])
# ...then split into separate objects
list2env(ld_N, envir = parent.frame())


##### Prepare variables ---------------------------------------------------------------------------

# Select relevant variables and remove duplicates
# duplicates might have arisen from the way several data.frame where merged together.

### Focal plot ----

# keep only seed pirce for farmers who actually purchased seeds.
vars_fp <- focalplot %>%
  filter(purchased_bin == 1) %>% 
  select(seedprice_hybrid, seedprice_local) %>%
  as.list

### Community ----

vars_cmty <- as.list(select(community, price_Urea, price_CAN, price_DAP,
                            price_pesticide, price_fungicide, price_herbicide))

vars_cmty_NPK <- as.list(select(community_NPK, N_price, P_price))

### Household ----

# Unequal repartition of cropping sytsem between zones
# Maize Pigeon pea intercrop over represented in the North 
# mpp > 3 mmc
table(ld[["household"]]$cropsys, ld[["household"]]$zone)

## Labour

household_lab <- household %>%
  select(hhid, plot_id, crop, cropsys, matches("tlab_"))

# Create unique index in household data.frame
household_lab$lab_id <- paste0(household_lab$hhid, household_lab$plot_id)
household_lab <- household_lab[!duplicated(household_lab$lab_id), ]

# Create list of all labour variables
vars_lab <- household_lab %>%
  filter(!is.na(cropsys)) %>%
  pivot_wider(names_from = "cropsys", 
              values_from = matches("^[a-z]lab_")) %>%
  select(matches("^[a-z]lab_")) %>%
  as.list()

### Maize prices ----


### Create summary tibble ----

vars_glb_l <- c(vars_fp, vars_cmty, vars_cmty_NPK, vars_lab)
vars_glb_l <- map(vars_glb_l, ~ .x[!is.na(.x)]) # Get rid of NA
vars_glb <- 
  tibble(param = names(vars_glb_l), values = vars_glb_l) %>%
  mutate(count = map_dbl(values, length),
         mean = map_dbl(values, mean),
         sd = map_dbl(values, sd))

vars_glb$lhc_bin <- map_int(vars_glb_l, ~ ifelse(length(.x) > 5, 1L, 0L))


##### Latin hypercube sampling --------------------------------------------------------------------

# Numbers of sample to derive for each variable
n <- 100

# Construct lhc sampling matrix
# each column corresponds to a parameter
nvar_lhc <- nrow(vars_glb) - sum(vars_glb$lhc_bin == 0)
lhc <- as.data.frame(lhs::randomLHS(n, nvar_lhc))
names(lhc) <- vars_glb$param[vars_glb$lhc_bin == 1]

# Use lhc sampling matrix to sample gamma ditributions
sampled_data_lhc <- map2_dfr(.x = vars_glb$values[vars_glb$lhc_bin == 1], 
                             .y = lhc, 
                             .f = ~ vars_sampling_gamma(.x, .y))
# Diagnosis plots
plot_sample_lhs(vars_glb$values[vars_glb$lhc_bin == 1], 
                sampled_data_lhc, 
                plot.dim = c(4, 5))

sampled_data_rd <- map_df(.x = vars_glb$values[vars_glb$lhc_bin == 0], 
                          .f = ~ sample(.x, size = n, replace = TRUE))

# For variables with a less than 5 data points 
# existing values are randomly sampled n times.
sampled_data <- cbind(sampled_data_lhc, sampled_data_rd)
sampled_data <- sampled_data[match(vars_glb$param, names(sampled_data))]

# Add unique row id
sampled_data$id <- 1:nrow(sampled_data)
sampled_data <- select(sampled_data, id, everything())


##### Save data -----------------------------------------------------------------------------------

write.csv(sampled_data, 
          file = "./data/sampled/TAMASA_sampled_vars.csv",
          row.names = FALSE)

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
