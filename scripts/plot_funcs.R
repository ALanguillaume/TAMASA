

#' Simple wrapper around ggplot histogram
#'
#' Can only deal with one variable

phist <- function(df, var, bins = 50, fill = "darkgreen", alpha = 0.5,lab_unit = ""){
  var <- rlang::enquo(var)
  ggplot(df) +
    aes(x = !!var) +
    geom_histogram(bins = bins,
                   fill = fill,
                   colour = "black",
                   alpha = alpha)+
    xlab(lab_unit)+
    ggtitle(sub("~", "", deparse(substitute(var))))
}

phist_string <- function(df, var, bins = 50, fill = "darkgreen", alpha = 0.5, lab_unit){
  
  
  if(fill == "darkgreen"){
    ggplot(df) +
      aes_string(x = var) +
      geom_histogram(bins = bins,
                     fill = fill,
                     colour = "black",
                     alpha = alpha)+
      labs(y = "", x = lab_unit)+
      ggtitle(var)
  } else {
    ggplot(df) +
      aes_string(x = var, fill = fill) +
      geom_histogram(bins = bins,
                     colour = "black",
                     alpha = alpha)+
      labs(y = "", x = lab_unit)+
      ggtitle(var)
  }
  
}

phist_matrix <- function(df, vars, bins = 100, dim = c(4,4), 
                         fill = "darkgreen", alpha = 0.5, lab_units){
  
  lp  <- as.list(rep(NA, length(vars)))
  for (i in seq_along(vars)) { 
    lp[[i]] <- phist_string(df, vars[[i]], bins = bins, fill = fill, alpha = alpha,
                            lab_unit = lab_units[i])
    
  }
  ggpubr::ggarrange(plotlist = lp, nrow = dim[1], ncol = dim[2],
                    common.legend = TRUE, legend="bottom")
}


plot_var <- function(df, var, plot.unit){
  var <- rlang::enquo(var)
  var_nms <- sub("~", "", deparse(substitute(var)))
  ggplot(df)+
    aes(y = !!var, x = 1)+
    geom_violin(fill = "darkgreen", alpha = 0.1, na.rm = TRUE)+
    ggbeeswarm::geom_quasirandom(color = "darkgreen", alpha = 0.5, 
                                 groupOnX = TRUE, na.rm = TRUE)+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+
    labs(x = var_nms, y = plot.unit)+
    ylim(0, NA)+
    ggtitle(var_nms)
}

plot_var_facet <- function(df, vars, plot.unit, title){
  pivot_longer(df, cols = vars,
               names_to = "category",
               values_to = "value") %>%
    plot_var(var = value, plot.unit = plot.unit)+
    facet_wrap(. ~ category)+
    xlab("")+
    ggtitle(title)
}



plot_var_string <- function(df, var, plot.unit, fill = "darkgreen", label.si = FALSE){
  
  if(fill == "darkgreen"){
    p <- ggplot(df)+
      aes_string(y = var, x = 1)+
      geom_violin(fill = "darkgreen", alpha = 0.1, na.rm = TRUE)+
      ggbeeswarm::geom_quasirandom(color = "darkgreen", alpha = 0.5, 
                                   groupOnX = TRUE, na.rm = TRUE)
  } else{
    p <- ggplot(df)+
      aes_string(y = var, x = fill, fill = fill, color = fill)+
      geom_violin(alpha = 0.1, na.rm = TRUE)+
      ggbeeswarm::geom_quasirandom(alpha = 0.5, 
                                   groupOnX = TRUE, na.rm = TRUE)
  }
  
  if(label.si == TRUE){
    p <- p + scale_y_continuous(labels = scales::label_number_si())
  }
  p +
    labs(x = "", y = plot.unit)+
    theme(axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none")+
    ggtitle(var)
}

plot_var_matrix <- function(df, vars, plot.unit, plot.dim, fill = "darkgreen", label.si = FALSE){
  
  lp <- pmap(.l = list(var = vars, plot.unit = plot.unit), 
             .f = plot_var_string, 
             df = df,
             fill = fill,
             label.si = label.si)
  
  ggpubr::ggarrange(plotlist = lp, 
                    nrow = plot.dim[1], 
                    ncol = plot.dim[2])
}


labour_category_plot <- function(df){
  
  dl <- df %>% 
    select(hh_index, plot_id, cropsys, mc8, matches("lab"), cropsys, ) %>% 
    pivot_longer(cols = str_subset(colnames(.), "lab"), 
                 names_to = "bar", 
                 values_to = "value") %>% 
    separate(bar, into = c("lab_cat", "task", "area"), sep = "\\_") %>%
    filter(str_detect(task, "^[a-z]{1}$"))
  
  dl$task <- fct_recode(dl$task, harvest = "h", 
                        "land \n preparation" = "l",
                        planting = "p",
                        fertilizing = "r",
                        weeding = "w")
  
  dl$lab_cat <- fct_recode(dl$lab_cat, 
                           family = "flab", 
                           hired = "hlab", 
                           total = "tlab")
  
  dl$cropsys <- fct_recode(dl$cropsys, "maize monoculture" = "mmc",
                           "maize/pigeon pea intercrop" = "mpp")
  
  ggplot(dl)+
    aes(y = value, x = lab_cat, color = lab_cat, group = lab_cat)+
    ggbeeswarm::geom_quasirandom(alpha = 0.5)+
    geom_violin(aes(fill = lab_cat), alpha = 0.5)+
    ggbeeswarm::geom_quasirandom(data = filter(dl, value == 0), 
                                 color = "black",
                                 groupOnX = FALSE)+
    facet_grid(task ~ cropsys +mc8)+
    theme(legend.position = "none",
          panel.spacing.y = unit(1, "lines"))+
    labs(y = expression(days.ha^-1), x = "Labour category")+
    ggtitle("Labour amount")
}

labour_price_plot <- function(df){
  
  dl <- df %>% 
    select(hh_index, plot_id, cropsys, mc8, matches("lab"),cropsys, ) %>% 
    pivot_longer(cols = str_subset(colnames(.), "lab"), 
                 names_to = "bar", 
                 values_to = "value") %>% 
    separate(bar, into = c("lab_cat", "task", "area"), sep = "\\_") %>%
    filter(str_detect(task, "^[a-z]{2}$"))
  
  dl$task <- fct_recode(dl$task, 
                        harvest = "hw", 
                        "land \n preparation" = "lw",
                        planting = "pw",
                        fertilizing = "rw",
                        weeding = "ww")
  
  dl$cropsys <- fct_recode(dl$cropsys, 
                           "maize monoculture" = "mmc",
                           "maize/pigeon pea intercrop" = "mpp")
  
  ggplot(dl)+
    aes(y = value, x = 1)+
    ggbeeswarm::geom_quasirandom(alpha = 0.5, 
                                 color = "darkgreen")+
    geom_violin(aes(fill = lab_cat), 
                alpha = 0.5, 
                fill = "darkgreen", 
                color = "darkgreen")+
    ggbeeswarm::geom_quasirandom(data = filter(dl, value == 0), 
                                 color = "black",
                                 groupOnX = FALSE)+
    facet_grid(task ~ cropsys + mc8)+
    scale_y_continuous(labels = scales::label_number_si())+
    theme(legend.position = "none",
          panel.spacing.y = unit(1, "lines"),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())+
    labs(y = expression(Tsh.ha^-1), x = "")+
    ggtitle("Hiring price")
}

labour_harvest_plot <- function(df){
  dl <- df %>% 
    select(matches("lab_h_dt$"), cropsys, hh_index, plot_id) %>% 
    pivot_longer(cols = str_subset(colnames(.), "lab"), 
                 names_to = "bar", 
                 values_to = "value") %>% 
    separate(bar, into = c("lab_cat", "task", "area"), sep = "\\_")
  
  ggplot(dl)+
    aes(y = value, x = lab_cat, color = lab_cat, group = lab_cat)+
    ggbeeswarm::geom_quasirandom(alpha = 0.5)+
    geom_violin(aes(fill = lab_cat), alpha = 0.5)+
    ggbeeswarm::geom_quasirandom(data = filter(dl, value == 0), 
                                 color = "black",
                                 groupOnX = FALSE)+
    facet_grid(task ~ cropsys)+
    theme(legend.position = "none",
          panel.spacing.y = unit(1, "lines"))+
    labs(y = expression(days.ha^-1), x = "Labour category")+
    ggtitle("Labour amount")
}
