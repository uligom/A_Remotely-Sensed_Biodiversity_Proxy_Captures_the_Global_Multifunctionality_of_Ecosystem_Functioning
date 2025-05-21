#### PLOT RaoQ vs GPPsat

### Author: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))

# Measure script run time
library(tictoc)
tic("Script run time")



### Options --------------------------------------------------------------------
# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
dat_in <- as.character(read.table("data/data_version.txt"))
vers_out <- dat_in


### Utilities ------------------------------------------------------------------
## Packages
library(RColorBrewer) # manipulate ggplot colors
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plot
library(ggrepel)      # repelled labels
library(patchwork)    # combine and arrange plots
library(readr)        # tidy read/save
library(scales)       # package for rescaling
library(tidyr)        # reorganize tibbles

## Functions
# source("scripts/functions/min_max_norm.R")
# source("scripts/functions/plot_scatterplot_models.R")
# source("scripts/functions/plot_best.R")
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### Data -----------------------------------------------------------------------
dat <- read_csv(glue::glue("data/inter/data4analysis_efps_{dat_in}.csv"), show_col_types = F) %>% 
  # dplyr::filter(!IGBP %in% c("DBF", "MF")) %>% # test with/without DBF/MF
  # dplyr::filter(!SITE_ID %in% c("HEAL", "TEAK", "TOOL")) %>% # test with/without outliers
  glimpse()



### Process data ---------------------------------------------------------------
dat <- dat %>%
  mutate(
    GROUP = if_else(condition = IGBP %in% c("DBF", "MF"),
                    true = "Dense forests",
                    false = "Sparse vegetation"),
    .after = IGBP
  ) %>%
  glimpse()



### Plot Sat diversity vs GPPsat -----------------------------------------------
## Bands ----
m1 <- dat %>% drop_na(GPPsat, Rao_Q_S2) %>% lm(formula = "GPPsat ~ Rao_Q_S2")
r2 <- summary(m1)$r.squared %>% magrittr::multiply_by(100) %>% round(digits = 1) %>% format(nsmall = 1)
labelcap <- bquote(R^2 ~ "=" ~ .(r2) ~ "%" ~
                     # " Intercept =" ~ .(signif(m1$coef[[1]], 2)) ~
                     # " Slope =" ~ .(signif(m1$coef[[2]], 2)) ~
                     "  p =" ~ .(signif(summary(m1)$coef[2,4], 2)) ~
                     "  n =" ~ .(nrow(drop_na(dat, GPPsat, Rao_Q_NIRv)))
)

p_scatter1 <- dat %>%
  drop_na(GPPsat, Rao_Q_S2) %>% 
  ggplot(aes(x = Rao_Q_S2, y = GPPsat)) +
  # facet_wrap(. ~ IGBP, scales = "free_x") + theme_facets + # test by groups
  geom_smooth(method = "lm", formula = 'y ~ x', na.rm = T, color = "gray25") +
  geom_point(aes(fill = IGBP), color = point_border_color, shape = point_shape, size = point_size_medium_small, na.rm = T) +
  scale_fill_manual(values = CatCol_igbp) +
  guides(fill = guide_legend(title = "IGBP class", override.aes = list(size = point_size_big))) +
  ggrepel::geom_label_repel(aes(label = SITE_ID), alpha = 0.5, max.overlaps = 5, na.rm = T) +
  labs(caption = labelcap) +
  xlab(expression(paste(RaoQ[bands], " [-]"))) + ylab(expression(paste(GPP[sat], " [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]"))) + # axis title
  theme_bw() +
  NULL


## NDVI ----
m1 <- dat %>% drop_na(GPPsat, Rao_Q_NDVI) %>% lm(formula = "GPPsat ~ Rao_Q_NDVI")
r2 <- summary(m1)$r.squared %>% magrittr::multiply_by(100) %>% round(digits = 1) %>% format(nsmall = 1)
labelcap <- bquote(R^2 ~ "=" ~ .(r2) ~ "%" ~
                     # " Intercept =" ~ .(signif(m1$coef[[1]], 2)) ~
                     # " Slope =" ~ .(signif(m1$coef[[2]], 2)) ~
                     "  p =" ~ .(signif(summary(m1)$coef[2,4], 2)) ~
                     "  n =" ~ .(nrow(drop_na(dat, GPPsat, Rao_Q_NIRv)))
)

p_scatter2 <- dat %>%
  drop_na(Rao_Q_NDVI, GPPsat) %>% 
  ggplot(aes(x = Rao_Q_NDVI, y = GPPsat)) +
  # geom_smooth(aes(color = GROUP), method = "lm", formula = 'y ~ x',
  #             alpha = alpha_transparent, fill = GROUP, na.rm = T) + # by groups
  # scale_color_manual(values = orange_teal_dark) +
  # scale_fill_manual(values = orange_teal_light) +
  # facet_wrap(. ~ IGBP, scales = "free_x") + theme_facets + # test by groups
  geom_smooth(method = "lm", formula = 'y ~ x', color = "gray25", na.rm = T) +
  geom_point(aes(fill = IGBP), color = point_border_color, shape = point_shape, size = point_size_medium_small, na.rm = T) +
  scale_fill_manual(values = CatCol_igbp) +
  guides(fill = guide_legend(title = "IGBP class", override.aes = list(size = point_size_big))) +
  ggrepel::geom_label_repel(aes(label = SITE_ID), alpha = 0.5, max.overlaps = 5, na.rm = T) +
  labs(caption = labelcap) +
  xlab(expression(paste(RaoQ[NDVI], " [-]"))) + ylab(expression(paste(GPP[sat], " [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]"))) + # axis title
  theme_bw() +
  theme(axis.title.y = element_blank() # remove y axis title text
        ) +
  NULL


## NIRv ----
m1 <- dat %>% drop_na(GPPsat, Rao_Q_NIRv) %>% lm(formula = "GPPsat ~ Rao_Q_NIRv")
r2 <- summary(m1)$r.squared %>% magrittr::multiply_by(100) %>% round(digits = 1) %>% format(nsmall = 1)
labelcap <- bquote(R^2 ~ "=" ~ .(r2) ~ "%" ~
                     # " Intercept =" ~ .(signif(m1$coef[[1]], 2)) ~
                     # " Slope =" ~ .(signif(m1$coef[[2]], 2)) ~
                     "  p =" ~ .(signif(summary(m1)$coef[2,4], 2)) ~
                     "  n =" ~ .(nrow(drop_na(dat, GPPsat, Rao_Q_NIRv)))
)

p_scatter3 <- dat %>%
  drop_na(Rao_Q_NIRv, GPPsat) %>%
  ggplot(aes(x = Rao_Q_NIRv, y = GPPsat)) +
  # facet_wrap(. ~ IGBP, scales = "free_x") + theme_facets + # test by groups
  geom_smooth(method = "lm", formula = 'y ~ x', na.rm = T, color = "gray25") +
  geom_point(aes(fill = IGBP), color = point_border_color, shape = point_shape, size = point_size_medium_small, na.rm = T) +
  scale_fill_manual(values = CatCol_igbp, guide = guide_legend(title = "IGBP class")) +
  guides(fill = guide_legend(title = "IGBP class", override.aes = list(size = point_size_big))) +
  labs(caption = labelcap) +
  ggrepel::geom_label_repel(aes(label = SITE_ID), alpha = 0.5, max.overlaps = 5, na.rm = T) +
  xlab(expression(paste(RaoQ[NIRv], " [-]"))) + ylab(expression(paste(GPP[sat], " [", mu,mol,CO[2]," ", m^{-2},s^{-1},"]"))) + # axis title
  theme_bw() +
  theme(axis.title.y = element_blank() # remove y axis title text
        ) +
  NULL


## Clean memory ----
rm(m1) # clean memory



### Combine plots --------------------------------------------------------------
p_gppsat_raoq <- (p_scatter1 | p_scatter2 | p_scatter3) +
  plot_layout(guides = 'collect') +
  plot_annotation(tag_levels = "a") &
  theme_combine +
  theme_transparent + theme(legend.key.size = unit(0, "mm")) + # for poster
  NULL


# p_gppsat_satbiodiv <- (p_scatter1 | p_scatter2 | p_scatter3 | p_scatter4) +
#   plot_layout(guides = 'collect') +
#   plot_annotation(tag_levels = "a") &http://127.0.0.1:42187/graphics/5a6c3768-0fe4-465b-b287-0f1471c0cd11.png
#   theme_combine
# p_gppsat_satbiodiv



### Save -----------------------------------------------------------------------
if (savedata) {
  scal <- 20
  width <- 31.8 * scal
  height <- 6.1 * scal
  
  ## GPPsat-raoQ scatterplots
  ggsave(filename = glue::glue("results/scatterplots/GPPsat-raoQ_{vers_out}.jpg"),
         plot = p_gppsat_raoq, device = "jpeg",
         width = 508, height = 285.75, units = "mm", dpi = 300)
  # transparent png
  ggsave(filename = glue::glue("results/scatterplots/GPPsat-raoQ_{vers_out}.png"),
         plot = p_gppsat_raoq, device = "png", bg = "transparent",
         width = width, height = height, units = "mm", dpi = 300)
  
  # ## GPPsat-raoQ by groups
  # ggsave(filename = glue::glue("results/scatterplots/GPPsat-raoQ_bands_byIGBP_{vers_out}.jpg"),
  #        plot = p_scatter1, device = "jpeg",
  #        width = 508, height = 285.75, units = "mm", dpi = 300)
  # ## GPPsat-raoQ by groups
  # ggsave(filename = glue::glue("results/scatterplots/GPPsat-raoQ_NDVI_byIGBP_{vers_out}.jpg"),
  #        plot = p_scatter2, device = "jpeg",
  #        width = 508, height = 285.75, units = "mm", dpi = 300)
  # ## GPPsat-raoQ by groups
  # ggsave(filename = glue::glue("results/scatterplots/GPPsat-raoQ_NIRv_byIGBP_{vers_out}.jpg"),
  #        plot = p_scatter3, device = "jpeg",
  #        width = 508, height = 285.75, units = "mm", dpi = 300)
}


### End ------------------------------------------------------------------------