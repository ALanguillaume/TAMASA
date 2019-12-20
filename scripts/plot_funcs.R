

#### General purpose plots ----

#' Produce plot for TAMASA
#' 
#' @param df data.frame containing the variables
#' @param var variable name as symbol not string !
#' @param plot.unit character, units of the corresponding variable

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


#' Produce plots with a common y axis
#' 
#' @param df data.frame containing the variables
#' @param var character vector with names of variables
#' @param plot.unit character, units of the corresponding variable
#' @param title character, title of the plot

plot_var_facet <- function(df, vars, plot.unit, title){
  pivot_longer(df, cols = vars,
               names_to = "category",
               values_to = "value") %>%
    plot_var(var = value, plot.unit = plot.unit)+
    facet_wrap(. ~ category)+
    xlab("")+
    ggtitle(title)
}


#' Matrix of plots with separate y-axis
#'
#' @param df data.frame containing the variables
#' @param vars character vector with names of variables
#' @param plot.unit character vector, units of the corresponding variable
#' @param fill character names of the variable that is used to assign colors to
#' elements of the plot.
#' @param label.si logical, if TRUE, number of f-axis labels are abbreviated using 
#' K for 10^{3} and M for 10^{6}.


plot_var_matrix <- function(df, vars, plot.unit, plot.dim, fill = "darkgreen", label.si = FALSE){
  
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
  
  lp <- pmap(.l = list(var = vars, plot.unit = plot.unit), 
             .f = plot_var_string, 
             df = df,
             fill = fill,
             label.si = label.si)
  
  ggpubr::ggarrange(plotlist = lp, 
                    nrow = plot.dim[1], 
                    ncol = plot.dim[2])
}


#### Functions dedicated to plot labour data -----


#' Plot labour per category: family/hired
#'
#' @param df data.frame containing labour data

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

#' Plot hired labour price
#'
#' @param df data.frame containing labour data

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

#' Plot labour uniquely for harvest
#'
#' @param df data.frame containing labour data

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
