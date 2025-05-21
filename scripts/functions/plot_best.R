plot_best <- function(data = dat, x = n_spp, y = RaoQ_S2_median, r2 = NA) {
  ## Utilities ----
  require(dplyr)
  require(rlang)
  require(ggplot2)
  require(ggrepel)
  require(magrittr)
  require(tidyr)
  source("scripts/themes/MyPlotSpecs.R")
  
  
  ## Quote ----
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  
  
  ## Plot ----
  p1 <- data %>%
    tidyr::drop_na(!!x, !!y) %>% 
    ggplot(aes(x = !!x, y = !!y)) +
    geom_smooth(method = "loess", formula = y ~ x, alpha = 0.25, color = "#1EB8B3", fill = "#2CDDD7", na.rm = T) +
    geom_smooth(method = "lm", formula = y ~ x, alpha = 0.25, color = "#D1400D", fill = "#F15620", na.rm = T) +
    geom_point(aes(color = IGBP), size = point_size_small, na.rm = T) +
    scale_color_manual(values = CatCol_igbp) +
    # ggrepel::geom_label_repel(aes(label = SITE_ID), na.rm = T) +
    labs(caption = glue::glue("R^2 = {r2}% (lm)")) +
    theme_bw() +
    theme(
      axis.text = element_text(size = text_size_medium),
      axis.ticks = element_blank(),
      axis.title = element_text(size = text_size_big),
      text = element_text(size = text_size_medium),
      title = element_text(size = text_size_big)
    )
  
  
  # ## Save ----
  # if (savedata) {
  #   ## Selected biodiversity scatterplots
  #   ggsave(filename = glue::glue("results/scatterplots/{as.character(x)}_vs_{as.character(y)}_{vers_out}.jpg"),
  #          plot = p1, device = "jpeg",
  #          width = 508, height = 285.75, units = "mm", dpi = 300)
  # }
  
  return(p1)
} # end of function

### Debug ----------------------------------------------------------------------
# debugonce(plot_best)