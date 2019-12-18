
lab_data = hh_plot$tlab_h_ha
# harvest_lab_corretcion <- function(lab_data, hours.min,){
  

  
  xmax <- 8/hours.min # assuming 2 hours working day
  xmin <- 1 # assuming 8 h working day
  
  d <- data.frame(lab_data, id = 1:length(lab_data))
  d <- d[order(lab_data, decreasing = T), ]
  d$x <- seq(xmax, xmin, length.out = length(lab_data))
  d$yc <- d$lab_data *d$x
  d <- d[order(d$id), ]
  
# }
  d <- mutate(d, y_dt = lab_data / hh_plot$yield, yc_dt = yc / hh_plot$yield)
# }
# d$id <- NULL

dl <- pivot_longer(d, cols = grep("id|x", colnames(d), invert = TRUE),
                   names_to = "var", 
                   values_to = "value")

ggplot(dl)+
  aes(y = value, x = var, group = var, color = var, fill = var)+
  geom_violin(alpha = 0.5, na.rm = TRUE)+
  ggbeeswarm::geom_quasirandom(alpha = 0.5, na.rm = TRUE, groupOnX = TRUE)

outlier_to_NA()


plot(x, y)

b <- (max(y) - min(y)) / (xmax - xmin)

a = min(y) - b

abline(a = a, b = b)


h <- b * x + a

d <- data.frame(x, y)

fit <- lm(y ~ I(x^5), d)

plot(x, y)

plot(x, predict(fit, newdata = d["y"]), type = "l")
