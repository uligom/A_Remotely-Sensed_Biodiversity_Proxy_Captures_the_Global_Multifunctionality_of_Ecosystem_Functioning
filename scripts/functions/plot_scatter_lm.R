### Function -------------------------------------------------------------------
plot_scatter_lm <- function(data = dat, x = GPPsat, y = Rao_Q_NIRv, group = NA) {
  ## Utilities ----
  require(dplyr)
  require(rlang)
  require(ggplot2)
  require(ggrepel)
  require(magrittr)
  require(tidyr)
  source("scripts/themes/MyPlotSpecs.R")
  source("scripts/themes/MyThemes.R")
  
  ## Quote ----
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  if (!is.na(group)) {group <- rlang::ensym(group)} else {group <- NULL}
  
  ## Model ----
  m1 <- data %>% drop_na(!!x, !!y) %>% lm(formula = paste0(as.character(x), " ~ ", as.character(y)))
  
  ## Labels ----
  r2 <- summary(m1)$r.squared %>% magrittr::multiply_by(100) %>% round(digits = 1) %>% format(nsmall = 1)
  labelcap <- bquote(R^2 ~ "=" ~ .(r2) ~ "%" ~
                       # " Intercept =" ~ .(signif(m1$coef[[1]], 2)) ~
                       # " Slope =" ~ .(signif(m1$coef[[2]], 2)) ~
                       "  p =" ~ .(signif(summary(m1)$coef[2,4], 2)) ~
                       "  n =" ~ .(nrow(drop_na(data, !!x, !!y)))
  )
  
  ## Plot ----
  p_scatter <- data %>%
    drop_na(!!x, !!y) %>% 
    ggplot(aes(x = !!y, y = !!x)) +
    geom_smooth(method = "lm", formula = 'y ~ x', na.rm = T, color = line_color_plot) +
    geom_point(color = point_border_color, fill = point_fill_color,
               shape = point_shape, size = point_size_medium_small, na.rm = T) +
    # scale_fill_manual(values = CatCol_igbp) +
    guides(fill = guide_legend(override.aes = list(size = point_size_big))) +
    # ggrepel::geom_label_repel(aes(label = SITE_ID), alpha = 0.5, max.overlaps = 5, na.rm = T) +
    labs(caption = labelcap) +
    # xlab(expression(paste(RaoQ[bands], " [-]"))) +
    # ylab(expression(paste(GPP[sat], " [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]"))) + # axis title
    theme_bw() +
    theme_print +
    theme(plot.caption = element_text(color = text_color_background)) +
    NULL
  
  if (!is.null(group)) {p_scatter <- p_scatter + geom_point(aes(fill = !!group))}
  
  ## Output ----
  return(p_scatter)
}

### Debug ----------------------------------------------------------------------
# debugonce(plot_scatter_lm)