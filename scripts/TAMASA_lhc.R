

require(ggplot2)
require(purrr)
require(dplyr)
require(here)
require(stringr)
require(tidyr)


TAMASA_prepare_lhc <- function(){
  

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
  
  cmty_maize_prc <- community %>% 
    select(starts_with("maiprc")) %>%
    as.list()
  
  hh_maize_prc <- household %>% 
    select(starts_with("mz_price")) %>%
    as.list()
  
  vars_maize_prc <- c(cmty_maize_prc, hh_maize_prc)
  
  
  ### Create summary tibble ----
  
  vars_glb_l <- c(vars_fp, vars_cmty, vars_cmty_NPK, vars_lab, vars_maize_prc)
  vars_glb_l <- map(vars_glb_l, ~ .x[!is.na(.x)]) # Get rid of NA
  vars_glb <- 
    tibble(param = names(vars_glb_l), values = vars_glb_l) %>%
    mutate(count = map_dbl(values, length),
           mean = map_dbl(values, mean),
           sd = map_dbl(values, sd))
  
  vars_glb$lhc_bin <- map_int(vars_glb_l, ~ ifelse(length(.x) > 5, 1L, 0L))
 
  return(vars_glb)
   
}

# Numbers of samples to derive for each variable
# n <- 100

TAMASA_sample_lhc <- function(vars_glb, n){
  
  ##### Latin hypercube sampling --------------------------------------------------------------------
  
  # Construct lhc sampling matrix
  # each column corresponds to a parameter
  nvar_lhc <- nrow(vars_glb) - sum(vars_glb$lhc_bin == 0)
  lhc <- as.data.frame(lhs::randomLHS(n, nvar_lhc))
  names(lhc) <- vars_glb$param[vars_glb$lhc_bin == 1]
  
  # Use lhc sampling matrix to sample gamma ditributions
  sampled_data_lhc <- map2_dfr(.x = vars_glb$values[vars_glb$lhc_bin == 1], 
                               .y = lhc, 
                               .f = ~ vars_sampling_gamma(.x, .y))
  
  sampled_data_rd <- map_df(.x = vars_glb$values[vars_glb$lhc_bin == 0], 
                            .f = ~ sample(.x, size = n, replace = TRUE))
  
  # For variables with a less than 5 data points 
  # existing values are randomly sampled n times.
  sampled_data <- cbind(sampled_data_lhc, sampled_data_rd)
  sampled_data <- sampled_data[match(vars_glb$param, names(sampled_data))]
  
  # Add unique row id
  sampled_data$id <- 1:nrow(sampled_data)
  sampled_data <- select(sampled_data, id, everything())
  
  return(sampled_data)
}


#' Perform sampling from a gamma distribution using quantile 
#' derive form a latin hypercube
#'
#' @param x numeric vector, variable to perfrom sampling for. 
#' The raw values of the varirable x are used to compute its mean and standard-deviation 
#' that are later used to derive the shape and scale of a gamma distribution.
#' @param x_lhc numeric vector, quantiles derived from latin hypercuve. Effectively 
#' one column of the matrix retrun by lhs::randomLHS()
#'
#' @return numeric vector of the same length as x_lhc containing the samples from 
#' the resulting gamma distribution.

vars_sampling_gamma <- function(x, x_lhc){
  mean_x = mean(x, na.rm = TRUE)
  sd_x = sd(x, na.rm = TRUE)
  sc = sd_x^2 / mean_x
  sh = mean_x^2 / sd_x^2 
  qgamma(x_lhc, shape = sh, scale = sc)
}



#### Latin hypercube diagnosis plots -----

#' Plot samples derived by lhc sampling
#' 
#' also incudes corresponding raw data points (displayed in red) and
#' raw data mean as an horizontal red line.
#' 
#' @param i numeric index of a given varaible to iterate through
#' the columns of the data.frames containing: the sampled values,
#' raw values and the mean.
#'

plot_sample_lhs_atom <- function(i, vars_df, sampled_data){
  ggplot(sampled_data)+
    aes_string(y = names(sampled_data)[i], x = 1)+
    geom_violin(alpha = 0.3) +
    ggbeeswarm::geom_quasirandom(alpha = 0.3) +
    geom_hline(yintercept = vars_df$mean[vars_df$lhc_bin == 1][i], color = "red")+
    expand_limits(y = 0)+
    ggbeeswarm::geom_quasirandom(data = data.frame(y = vars_df$values[vars_df$lhc_bin == 1][[i]]), 
                                 aes(y = y), 
                                 color = "red",
                                 size = 1.5)+
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank())
}


TAMASA_plot_lhc <- function(vars_glb, sampled_data){
  vars_df <- vars_glb$values[vars_glb$lhc_bin == 1]
  lp <- map(seq_along(sampled_data), plot_sample_lhs_atom, vars_df, sampled_data)
  ggpubr::ggarrange(plotlist = lp, nrow = 6, ncol = 5)
}
