
#' Make list of TAMASA datasets used per chapter
#'
#' @param file character path to the .Rmd that contains chapter code.
#' @return data.frame containing list of datasets used in a given chapter.

list_datasets <- function(file){
  
  path <- here::here(file)
  df <- readLines(path) %>%
    str_subset("TZAPS17.") %>%
    str_subset("read") %>%
    str_extract("TZAPS17.*\\.[a-z]*") %>% 
    enframe() %>%
    rename(Description = name,
           File = value) 
  
  df <- df[!duplicated(df$File), ]
  
  return(df)
}

#' Creates conversion dictionaries used to process TAMASA data
#'
#' @return list of data.frame
#' \itemize{
#' \item unit : units for physical quantities (weight and volume)
#' \item funit : units for fertilizer weight
#' \item seedsource : seed provenance
#' \item seedtype : type of seeds
#' \item inp : nutrient concentrations of different fertilizers
#' \item areaunit : areal units
#' \item landuse : landuse categories
#' \item fieldshare : percentage of field shared
#' }

create_conversion_dics <- function() {
  
  unit <- data.frame(
    cat = c(NA, 0:25), 
    kg_maize = c(0, 120, 100, 70, 50, 25, 10, 5, 2, 1, 1000, 15.2, 7.6, 3.8, 0.76, rep(NA, times = 12)),
    kg_pp = c(0, 120, 100, 70, 50, 25, 10, 5, 2, 1, 1000, 17.4, 8.7, 4.4, 0.87, rep(NA, times = 12)),
    kg = c(0, 120, 100, 70, 50, 25, 10, 5, 2, 1, 1000, rep(NA, times = 16)),
    unit = c(
      NA, "120 kg", "100 kg", "70 kg", "50 kg", "25 kg", "10 kg",
      "5 kg", "2 kg", "1 kg", "1000 kg", "debe", "10 l", "5 l", "1 l",
      "crate", "tray", "grams", "bunch", "wheelbarrow", "cart",
      "canter", "pickup", "bale", "piece", "day", "acre"
    )
  )
  
  # Fertilizer units
  
  funit <- data.frame(
    cat = c(1:6, 99),
    kg = c(1, 25, 50, 100, 4, 20, NA),
    unit = c("Kilogram", "25 kg bag", "50 kg bag", "100 kg bag", "pishi (4kg sado)",
             "debe", "Other unit")
  )
  
  # Labels for seed source
  
  seedsource <- data.frame(
    seed6 = c(1:10, 99), # categories
    source = c(
      "small trader", "stockist/ agent", "large company", "NGO/ CBO",
      "cooperative", "other farmer group", "own production",
      "own production, other field", "other farmer",
      "extension agent", "other"
    )
  )
  
  # Labels for seed type
  
  seedtype <- data.frame(
    seed2 = 1:4, # categories
    type = c(
      "improved, purchased", "improved, recycled",
      "local, purchased", "local, recycled"
    )
  )
  
  # Nutrient concentrations of different fertilizers
  inp <- readxl::read_xlsx(path = here::here("./data/fertilization/nutrient concentrations fertilizers.xlsx"),
                           sheet = 1,
                           range = readxl::cell_cols(1:5),
                           col_types = c("numeric", "text", rep("numeric",3)),
                           na = c("", "NA")
  ) %>% as.data.frame()
  
  areaunit <- data.frame(
    cat = 1:3,
    hectares = c(0.404685642, 1, 1 / 10000),
    unit = c("Acres", "Hectares", "Square meters")
  )
  
  landuse <- data.frame(
    cat = c(1:8, 99),
    lu = c("Cropped", "Fallow", "Grazed", "Rented out", "Loaned out", "Garden",
           "Woodlot", "Virgin", "Other")
  )
  
  fieldshare <- data.frame(cat = 1:5, fs = c(1, 0.75, 0.5, 0.25, NA))
  
  return(mget(ls()))
  
}


#' Convert maize selling price to Tsh/kg
#'
#' Maize selling price are expressed in a variety of different units/Tsh
#' This function uses the \code{dics$unit} dictionary to perform conversions.
#' 
#' @param df data.frame containing the variables to convert.
#' @param from character vector specifying the input column names.
#' @param to character vector specifying the output column names.
#'
#' @return data.frame with both inputs and converted variables.

convert_prices_tsh_kg <- function(df, from, to){
  
  for (i in seq_along(from)){ 
    
    from_tsh <- str_replace(string = from[i], 
                            pattern = "b", 
                            replacement = "a")
    
    df <- merge(x = df, 
                y = dics$unit[, c("cat", "kg_maize")],
                by.x = from[i],
                by.y = "cat",
                all = TRUE)
    
    df[[to[i]]] <- df[[from_tsh]] / df$kg_maize
    
    df$kg_maize <- NULL
    
  }
  return(df)
}

#' Convert outliers to NA
#'
#'
#' @param x  numeric vector, (not one column data.frame), somthing of the form df$var.
#' @param thershold numeric, threshold above which a data point is considered to be an outlier.
#' @param quantile numeric, upper quantile above which data poins  should be considered as outliers.
#' @param invert logical, ... works only in combintation with argument threshold.

outlier_to_NA <- function(x, threshold, quantile, invert = FALSE){ 
  
  if(!missing(threshold) && !missing(quantile)){
    stop("Please specify either threshold or quantile not both")
  }
  
  if(!missing(threshold) && missing(quantile)){
    
    if(invert == TRUE){
      x[which(x < threshold)] <- NA
    } else{
      x[which(x > threshold)] <- NA
    }
  }
  
  if(missing(threshold) && !missing(quantile)){
    th <- quantile(x, probs = quantile, names = FALSE, na.rm = TRUE)
    x[which(x > th)] <- NA
    
  }
  return(x)
}


#' Sum labour amount over worker type.
#'
#' This function is a wrapper around \code{sum_category} that allows to sum 
#' labour amount accross worker type while handling the non-consistent 
#' labelling of the variables. FYI : the worker type is indicated by suffix2 
#' in labour variables names for the household survey.
#' 
#' @param lab_cat list of data.frame, each data.frame correspond to a work category
#'  either hired or family. 
#'
#' @return a list of data.frame with labour variables summed over worker type.

sum_worker_type <- function(lab_cat){
  
  hn <- str_extract(colnames(lab_cat), "^[:lower:]*(?=_)")
  
  if(length(unique(hn)) != 1){
    stop("Prefix not identical for all labour variables")
  }
  
  p <- unique(hn)
  
  if(p == "flab"){
    pattern <- "^.lab_.(?!f)"
  } else if(p == "hlab"){
    pattern <- "^.lab_.(?!w)"
  } else {
    stop(sprintf("Wrong labour category: %s.\n
                 Only flab (family) and hlab (hired) allowed.", p))
  }
  
  lab_df <- sum_category(lab_cat, pattern)
  
  return(lab_df)
}

#' Sum a labour data.frame over a given category
#' 
#' @param df data.frame, containing labour variables as specified in hh_plotml.tab.
#' @param pattern character string consisting of a regex to match the prefix / suffix 
#' in labour variable name corresponding to the desired labour type.
#'
#' @return data.frame with labour variables summed over the desired  labour type.

sum_category <- function(df, pattern, na.rm = FALSE){
  
  split.default(x = df,
                f = str_extract(colnames(df), pattern)) %>%
    purrr::map_dfr(rowSums_smart_NA) 
}

#' Missing value pattern in labour data
#'
#' Used in the development process
#' 
#' @param df_lab data.frame containing labour data
#'

pattern_NA_lab <- function(df_lab) {
  split.default(x = df_lab,
                f = str_extract(colnames(df_lab), 
                                "^.lab_.(?!f)")) %>% 
    map(., ~ modify(.x, is.na)) %>%
    map(rowSums) %>%
    map(table) %>%
    reduce(rbind) %>%
    colnames()
}


#' rowSums with special NA handling
#'
#' rowSums that behaves as rowSums(, na.rm = TRUE)
#' except when input is a vector containing only NA.
#' In that case it returns NA instead of 0.
#'
#' @param df data.frame
#'

rowSums_smart_NA <- function(df){
  s <- rep(NA, nrow(df))
  for (i in 1:nrow(df)) {
    if(sum(is.na(df[i, ])) != ncol(df)){
      s[i] <- sum(df[i,], na.rm = TRUE)
    } 
  }
  return(s)
}


#' Replace an arbitrary string by 0.
#'
#' This is simply loop hiding, nothing fancy here.
#'
#' @param df data.frame of interest
#' @param fert character vector, names of the columns containing fertlizer input data.
#' @param na_string character which to replace by 0.

zero_if_no_input <- function(df, fert, na_string){
  for (f in fert) df[[f]][df$input == na_string] <- 0
  return(df)
}  


#' Add NPK fertilizer input 
#'
#' Basically adding NPK variable together with their respective counters parts prefixed by 'r'
#' 
#' @param df data.frame containing fertilizer data.
#' 
#' @return data.frame with one unique column by element (NPK)

# test foo <- fp_fert[c(1, 69, 313, 331), ]

add_fert_input <- function(df){
  
  fert_type <- c("N", "P", "K")
  
  for (ft in fert_type) {
    
    rft <- paste0("r", ft) 
    
    x <- which(!is.na(df[[ft]]) & !is.na(df[[rft]]))
    df[[ft]][x] <- rowSums(cbind(df[[ft]][x], df[[rft]][x])) 
    
  }
  
  return(df)
}


#' Calculate input price at community level
#'
#' Input price is calculated as such: 
#' (price_fert [Tsh] / (amount_fert [kg] * ratio_kg [kg])) + transp_price [Tsh/kg],
#' for each 
#' 
#' @param df data.frame containing the original varianles
#' @param input_type character vector
#' @param input_id
#' @param input_name
#' @param units
#' 

price_input <- function(df, input_type, input_id, input_name, units){
  
  for(u in units){
    
    for (i in input_id) {
      
      price_input <- paste0("price_", input_type)
      
      # index units used to measure fertilizer supply
      id <- which(df[, paste0(price_input, i, "_unit")] == u)
      
      price_fert <- df[id, paste0(price_input, i, "_tsh")]
      amount_fert <- df[id, paste0(price_input, i, "_amt")]
      ratio_kg <- dics$funit$kg[which(dics$funit$cat == u)]
      transp_price <- df[id, "transp_price"]
      
      df[id, paste0("price_", input_name[i])] <- 
        (price_fert / (amount_fert * ratio_kg)) + transp_price
    }
  }
  return(df)
}


#' Return names of the variables containing any negative value.
#'
#' @param df, data.frame to be checked for negative values.
#'
#' @return character vector with the names of the columns harbouring at least one negative value.

any_negative <- function(df){
  num <- map_lgl(df, is.numeric)
  nv <- names(which(map_lgl(df[,num], ~ any(.x < 0))))
  if(length(nv) != 0){
    return(nv)
  } else {
    print("No variable with negative values")
  }
}


#' Check if there is no duplicate within one vector.
#'
#' @param x numeric or character vector or factor.
#'
#' @return boolean, TRUE if all elements are unique else FALSE.

check_unique <- function(x) length(x) == length(unique(x))


#' Check if a data.frame as duplicated rows.
#'
#' @param df data.frame.
#' 
#' @return TRUE if there is duplicated rows in df FALSE otherwise.

dup_row <- function(df) nrow(df) != nrow(dplyr::distinct(df))


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

