# data:     data (avoid big data)
# labels:   unused

plot_sat_time <- function(data = NULL, timestamp = NULL, y = NULL, labels = NULL) {
  ### Utilities ----------------------------------------------------------------
  require(ggplot2)
  require(rlang)
  
  
  
  ### Quote --------------------------------------------------------------------
  t_sym <- rlang::ensym(timestamp)
  y_sym <- rlang::ensym(y)
  
  
  # ### Labels -------------------------------------------------------------------
  # labshorten <- function(labstrings) {
  #   format(labstrings, digits = 2)
  #   labstrings
  # }
  #
  ### Plot ---------------------------------------------------------------------
  data %>% 
    ggplot(aes(x = !!t_sym, y = !!y_sym)) +
    geom_line(size = 1) +
    facet_wrap(~ cell, # subplots of random pixels
               labeller = labeller(cell = labels)) + # title date for each subplot
    theme_minimal() +
    theme(axis.text.x = element_text(size = 10, angle = 270),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 20),
          panel.border = element_rect(colour = "black", fill = NA), # facet plotting panels
          strip.background = element_rect(colour = "black", fill = NA) # facet title panels
          ) +
    NULL
}