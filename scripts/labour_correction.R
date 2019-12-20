
library(tidyverse)
library(here)
source(here("./scripts/funcs.R"))
source(here("./scripts/plot_funcs.R"))

d_raw <- read_csv(here("./data/extracted/household.csv"), na = "NA")
lab_data = d_raw$tlab_h_ha

# harvest_lab_corretcion <- function(lab_data, hours.min,){
hours.max = 8
hours.min = 2

  
  xmax <- 1
  xmin <- hours.min / hours.max 
  
  d <- data.frame(lab_data, id = 1:length(lab_data))
  d <- d[order(lab_data, decreasing = FALSE), ]
  d$x <- seq(xmax, xmin, length.out = length(lab_data))
  d$yc <- d$lab_data * d$x
  d <- d[order(d$id), ]
  
# }
  d <- mutate(d, y_dt = lab_data / d_raw$yield, yc_dt = yc / d_raw$yield)
# }
# d$id <- NULL

d %>% 
  pivot_longer(cols = grep("id|x", colnames(d), invert = TRUE),
               names_to = "var", 
               values_to = "value") %>%
  ggplot()+
  aes(y = value, x = var, group = var, color = var, fill = var)+
  geom_violin(alpha = 0.5, na.rm = TRUE)+
  ggbeeswarm::geom_quasirandom(alpha = 0.5, na.rm = TRUE, groupOnX = TRUE)

d <- modify_at(d, c("y_dt", "yc_dt"), outlier_to_NA, quantile = 0.95)

d %>% 
  pivot_longer(cols = grep("id|x", colnames(d), invert = TRUE),
               names_to = "var", 
               values_to = "value") %>%
  ggplot()+
  aes(y = value, x = var, group = var, color = var, fill = var)+
  geom_violin(alpha = 0.5, na.rm = TRUE)+
  ggbeeswarm::geom_quasirandom(alpha = 0.5, na.rm = TRUE, groupOnX = TRUE)
