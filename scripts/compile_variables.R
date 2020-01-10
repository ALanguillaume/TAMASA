
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

### Focal plot
vars_fp <- focalplot %>%
  filter(purchased_bin == 1) %>%
  select(seedprice_hybrid, seedprice_local) %>%
  as.list

### Community
vars_cmty <- as.list(select(community, price_Urea, price_CAN, price_DAP,
                            price_pesticide, price_fungicide, price_herbicide))

vars_cmty_NPK <- as.list(select(community_NPK, N_price, P_price))

### Household

# Unequal repartition of cropping sytsem between zones
# Maize Pigeon pea intercrop over represented in the North 
# mpp > 3 mmc
table(ld[["household"]]$cropsys, ld[["household"]]$zone)

## Labour

household_lab <- household %>%
  select(hhid, plot_id, crop, cropsys, matches("^[a-z]lab_"))

household_lab$lab_id <- paste0(household_lab$hhid, 
                               household_lab$plot_id)

household_lab <- household_lab[!duplicated(household_lab$lab_id), ]

vars_lab <- household_lab %>%
  filter(!is.na(cropsys)) %>%
  # select(cropsys, matches("^[a-z]lab_")) %>%
  pivot_wider(names_from = "cropsys", 
              values_from = matches("^[a-z]lab_")) %>%
  select(matches("^[a-z]lab_")) %>%
  as.list()

foo <- map(vars_lab, ~ .x[!is.na(.x)]) %>% map_dbl(length)


data.frame(param = names(foo), value = foo) %>%
  separate(param, into = c("type", "task", "unit", "cropsys"), sep = "_")


vars_hh <- as.list(select(household_lab, matches("^[a-z]lab_")))

## Maize prices


#### Contrust data.frame with variables names, values and summary statistics

vars_glb_l <- c(vars_fp, vars_cmty, vars_cmty_NPK)
vars_glb_l <- map(vars_glb_l, ~ .x[!is.na(.x)])

vars_glb <- 
  tibble(param = names(vars_glb), values = vars_glb) %>%
  mutate(count = map_dbl(values, length),
         mean = map_dbl(values, mean),
         sd = map_dbl(values, sd))


##### Latin hypercube sampling --------------------------------------------------------------------

# Numbers of sample to derive for each variable
n <- 100

### Construct lhc sampling matrix
# each column corresponds to a parameter
lhc <- as.data.frame(lhs::randomLHS(n, nrow(vars_glb)))
names(lhc) <- vars_glb$param

# Use lhc sampling matrix to sample gamma ditributions
sampled_data <- map2_dfr(.x = vars_glb$values, 
                         .y = lhc, 
                         .f = ~ vars_sampling_gamma(.x, .y))
# Diagnosis plots
lp <- map(seq_along(sampled_data), plot_sample_lhs)
ggpubr::ggarrange(plotlist = lp, nrow = 2, ncol = 5)

