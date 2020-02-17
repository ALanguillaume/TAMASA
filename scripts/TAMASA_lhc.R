
# Load require packages 
packages <- c("lhs","ggplot2", "purrr", "dplyr", "here", "stringr","tidyr")
invisible(lapply(X = packages, 
       FUN = function(x) suppressPackageStartupMessages(require(x, character.only = TRUE))
       )) 

                
##### Latin hypercube sampling ---------------------------------------------------------------------



#' Select relevant variables in TAMASA data   
#' 
#' Automatically retireves the variables of interest from the processed TAMASA data files,
#' removed missing values, and gather variables as a list column within a summary data.frame.
#'
#' This data.frame serves as input to TAMASA_prepare_lhc()
#'   
#' It contains the following columns:
#' - param, character, variable name.
#' - values, list column with all values .
#' - count, numeric, number of data points per variable.
#' - mean, numeric, average value.
#' - sd, numeric, standard deviation.
#' - lhc_bin, integer, indicating if lhc sampling shoudl be perform for this variable (1) or not (0).

TAMASA_prepare_lhc <- function(){
  

  ##### Load data -----
  
  # load all extracted data
  lf <- list.files("./data/extracted/")
  # List with all data files...
  ld <- map(here("./data/extracted/", lf), ~ read.csv(.x))
  names(ld) <- str_remove(lf, "\\.csv")
  # keep only data for the Northern Zone
  ld_N <- map(ld, ~ .x[.x$zone == "N", ])
  # ...then split into separate objects
  list2env(ld_N, envir = parent.frame())
  
  
  ##### Prepare variables -----
  
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
  
  vars_maize_prc <- household %>% 
    select(starts_with("mz_price")) %>%
    unlist(use.names = FALSE) %>%
    list(hh_maize_prc= .)
  
  
  ### Create summary tibble ----
  
  vars_df_l <- c(vars_fp, vars_cmty, vars_cmty_NPK, vars_lab, vars_maize_prc)
  vars_df_l <- map(vars_df_l, ~ .x[!is.na(.x)]) # Get rid of NA
  vars_df <- 
    tibble(param = names(vars_df_l), values = vars_df_l) %>%
    mutate(count = map_dbl(values, length),
           mean = map_dbl(values, mean),
           sd = map_dbl(values, sd))
  
  vars_df$lhc_bin <- map_int(vars_df_l, ~ ifelse(length(.x) > 5, 1L, 0L))
 
  return(vars_df)
   
}



#' Performs Latin hypercube sampling
#' 
#' @param vars_df, summary data.frame as prepared by TAMASA_prepare_lhc().
#' @param n interger, numbers of samples to derive for each variable.
#' 
#' If the number of data points, `vars_df$count`, is less than 5, `vars_df$lhc_bin` will be equal to 0. 
#' In that case, the exisiting data points will just be randomly sampled `n` times. 
#' 

TAMASA_sample_lhc <- function(vars_df, n){
  
  ##### Latin hypercube sampling -----
  
  # Construct lhc sampling matrix
  # each column corresponds to a parameter
  nvar_lhc <- nrow(vars_df) - sum(vars_df$lhc_bin == 0)
  lhc <- as.data.frame(lhs::randomLHS(n, nvar_lhc))
  names(lhc) <- vars_df$param[vars_df$lhc_bin == 1]
  
  # Use lhc sampling matrix to sample gamma ditributions
  sampled_data_lhc <- map2_dfr(.x = vars_df$values[vars_df$lhc_bin == 1], 
                               .y = lhc, 
                               .f = ~ vars_sampling_gamma(.x, .y))
  
  sampled_data_rd <- map_df(.x = vars_df$values[vars_df$lhc_bin == 0], 
                            .f = ~ sample(.x, size = n, replace = TRUE))
  
  # For variables with a less than 5 data points 
  # existing values are randomly sampled n times.
  sampled_data <- cbind(sampled_data_lhc, sampled_data_rd)
  sampled_data <- sampled_data[match(vars_df$param, names(sampled_data))]
  
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
#' 
#' @note This function is only meant to be used internally by TAMASA_sample_lhc.

vars_sampling_gamma <- function(x, x_lhc){
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  sc <- sd_x^2 / mean_x
  sh <- mean_x^2 / sd_x^2 
  qgamma(x_lhc, shape = sh, scale = sc)
}


#### Latin hypercube diagnosis plots ---------------------------------------------------------------


#' Plot samples derived by lhc sampling
#' 
#' @param vars_df data.frame as produced by TAMASA_prepare_lhc().
#' @param sampled_data data.frame as produced by TAMASA_sample_lhc().


TAMASA_plot_lhc <- function(vars_df, sampled_data){
  
  l_sampled_vars <- map2(.x = sampled_data[ , -1], 
                         .y = names(sampled_data)[-1], 
                         ~ tibble::enframe(.x, name = NULL,  value = .y))
  
  lhc_plot_core <- function(v){
    
    var_name <- names(v)
    dmean <- vars_df$mean[vars_df$param == var_name]
    draw <- tibble(y = unlist(vars_df$values[vars_df$param == var_name]))
    
    ggplot(v)+
      aes_string(y = var_name, x = 1)+
      geom_violin(alpha = 0.3) +
      ggbeeswarm::geom_quasirandom(alpha = 0.3) +
      geom_hline(yintercept = dmean , color = "red")+
      expand_limits(y = 0)+
      ggbeeswarm::geom_quasirandom(data = draw,
                                   aes(y = y),
                                   color = "red",
                                   size = 1.5)+
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank())
  }
  
  lp <- map(l_sampled_vars, lhc_plot_core)
  ggpubr::ggarrange(plotlist = lp, nrow = 4, ncol = 6)
  
}