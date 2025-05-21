plot_sat_roi <- function(data = NULL, timestamp = NULL, roi = NULL, labels = NULL) {
  ### Utilities ----------------------------------------------------------------
  require(ggplot2)
  require(rlang)
  require(stars)
  
  
  
  ### Quote --------------------------------------------------------------------
  t_sym <- rlang::ensym(timestamp)
  
  
  
  ### Plot ---------------------------------------------------------------------
  ggplot() +
    stars::geom_stars(data = data) + # plot random subsets
    coord_equal() +
    facet_wrap(~ eval_tidy(t_sym), # subplots of random timestamps
               labeller = labeller(t_sym = labels)) + # title date for each subplot
    scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "#E2A2BE") +
    geom_sf(aes(color = "region of interest"), data = roi, show.legend = "polygon", inherit.aes = F,
            fill = NA, size = 1) + # plot region of interest
    scale_color_discrete(type = "black") +
    #guides(colour = guide_legend(override.aes = list(shape = 21))) + # change legend shape (NOT WORKING) https://stackoverflow.com/questions/13456765/ggplot2-custom-legend-shapes
    labs(fill = "NDVI", colour = "") +
    theme_void() +
    # theme(legend.background = element_rect(fill = "transparent"),
    #       legend.box.background = element_rect(fill = "transparent"),
    #       panel.background = element_rect(fill = "transparent"),
    #       panel.grid.major = element_blank(),
    #       panel.grid.minor = element_blank(),
    #       plot.background = element_rect(fill = "transparent", color = NA)
    #       ) + # transparent background
    NULL
}
# debugonce(plot_sat_roi)