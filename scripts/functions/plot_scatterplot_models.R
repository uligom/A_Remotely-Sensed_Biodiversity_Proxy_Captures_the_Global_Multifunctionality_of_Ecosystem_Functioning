plot_scatterplot_models <- function(
    data, x, y, color = NULL, label = "SITE_ID",
    mypal = c("#586158", "#C46B39", "#4DD8C0", "#3885AB", "#9C4DC4",
              "#C4AA4D", "#443396", "#CC99CC", "#88C44D", "#AB3232")
      ) {
  ## Utilities ----
  require(dplyr)
  require(ggplot2)
  require(ggrepel)
  require(rlang)
  
  
  ## Quote ----
  x <- rlang::ensym(x)
  y <- rlang::ensym(y)
  color <- if (is.null(color)) {color <- "gray10"} else {rlang::sym(color)}
  label <- if (is.character(label)) {rlang::sym(label)}
  
                       
  ## Plot ----
  plot0 <- data %>%
    ggplot(aes(x = !!x, y = !!y)) +
    geom_smooth(method = "loess", formula = y ~ x, na.rm = T, color = "#1EB8B3", fill = "#2CDDD7") +
    geom_smooth(method = "glm", formula = y ~ x, na.rm = T, color = "#F15620", fill = "#D1400D") +
    geom_point(aes(color = !!color), alpha = 0.67, size = 5, na.rm = T) +
    scale_color_manual(values = mypal) +
    geom_label_repel(aes(label = !!label), na.rm = T) +
    theme_classic() +
    theme(legend.title = element_blank(),
          legend.text  = element_text(color = "gray50", size = 14),
          axis.text    = element_text(color = "gray50", size = 20),
          axis.ticks   = element_blank(),
          axis.title   = element_text(size = 24),
          panel.background = element_blank(),
          panel.grid   = element_line(color = "gray75")
    )
  if (color == "gray10") {plot0 <- plot0 + theme(legend.position = "none")} # remove legend if no color (group) is given
  
  
  ## Output ----
  return(plot0)
}
# debugonce(plot_scatterplot_models)