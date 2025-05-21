### Author: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)

source("scripts/themes/MyPlotSpecs.R")

theme_combine <- theme(
  axis.text = element_text(size = text_size_medium),
  axis.title = element_text(size = text_size_big),
  legend.key.size = unit(12, "mm"), # space out legend elements
  legend.text = element_text(size = text_size_medium + 3),
  legend.title = element_text(face = "bold", size = text_size_big),
  # plot.margin = unit(c(0, 0, 0, 0), "cm"), # remove margins around individual plots
  strip.text = element_text(size = text_size_big), # facet title strips
  strip.background = element_rect(fill = "white"),
  text = element_text(size = text_size_medium),
  title = element_text(size = text_size_big)
)

theme_print <- theme(
  axis.line = element_blank(),
  axis.text = element_text(size = 24),
  axis.ticks = element_blank(), # remove ticks from empty axis
  axis.title = element_text(size = 32, face = "bold"),
  legend.text = element_text(size = 24),
  legend.title = element_text(size = 32, face = "bold"),
  # legend.position = c(0.8, 0.1),
  legend.direction = "vertical",
  legend.spacing = unit(0.5, "cm"),
  panel.border = element_rect(fill = NA),
  panel.spacing = unit(0.5, "cm"),
  # plot.margin = unit(c(0, 1, 0, 0), "cm"), # avoid blank space around plot
  plot.caption = element_text(size = 32),
  # plot.title = element_text(size = 40, face = "bold"),
  strip.text = element_text(size = 32, face = "bold"), # facet title strips
  strip.background = element_blank()
)

theme_facets <- theme(
  axis.line = element_blank(),
  axis.text.x = element_text(size = 20),
  axis.text.y = element_text(size = 24), # 'element_blank()' removes values on axis (when actual values are printed)
  axis.ticks = element_blank(), # remove ticks from empty axis
  axis.title = element_text(size = 26),
  legend.text = element_text(size = 24),
  legend.title = element_text(size = 28),
  # legend.position = c(0.8, 0.1),
  legend.direction = "vertical",
  panel.border = element_rect(fill = NA),
  panel.spacing = unit(0.5, "cm"),
  # plot.margin = unit(c(0, 1, 0, 0), "cm"), # avoid blank space around plot
  plot.caption = element_text(size = 28),
  # plot.title = element_text(size = 40, face = "bold"),
  strip.text = element_text(size = 24), # facet title strips
  strip.background = element_blank()
)

theme_less_facets <- theme(
  axis.line = element_blank(),
  axis.text = element_text(size = 24),
    axis.ticks = element_blank(), # remove ticks from empty axis
  axis.title = element_text(size = 36, face = "bold"),
  legend.text = element_text(size = 32),
  legend.title = element_text(size = 36, face = "bold"),
  # legend.position = c(0.8, 0.1),
  legend.direction = "vertical",
  legend.spacing = unit(0.5, "cm"),
  panel.border = element_rect(fill = NA),
  panel.spacing = unit(0.5, "cm"),
  # plot.margin = unit(c(0, 1, 0, 0), "cm"), # avoid blank space around plot
  plot.caption = element_text(size = 32),
  # plot.title = element_text(size = 40, face = "bold"),
  strip.text = element_text(size = 32, face = "bold"), # facet title strips
  strip.background = element_blank()
)

theme_transparent <- theme(
  panel.background = element_rect(fill = 'transparent'), # transparent panel bg
  plot.background = element_rect(fill = 'transparent', color = NA), # transparent plot bg
  # panel.grid.major = element_blank(), # remove major gridlines
  # panel.grid.minor = element_blank(), # remove minor gridlines
  legend.background = element_rect(color = 'transparent', fill = 'transparent'), # transparent legend bg
  legend.box.background = element_rect(color = 'transparent', fill = 'transparent'), # transparent legend panel
  legend.key = element_rect(color = 'transparent', fill = 'transparent'), # transparent legend keys
  strip.background = element_rect(color = 'transparent', fill = 'transparent') # transparent facet titles' background
)