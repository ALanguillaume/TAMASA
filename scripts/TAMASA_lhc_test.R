

source("./scripts/TAMASA_lhc.R")


vars_glb <- TAMASA_prepare_lhc() 

sampled_data <- TAMASA_sample_lhc(vars_glb, n = 100)

TAMASA_plot_lhc(vars_glb, sampled_data)
vars_df <- vars_glb$values[vars_glb$lhc_bin == 1]
lp <- map(seq_along(sampled_data), plot_sample_lhs_atom, vars_df, sampled_data)
ggpubr::ggarrange(plotlist = lp, nrow = 5, ncol = 5)
