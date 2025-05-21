### CALCULATE MULTIFUNCTIONALITY ###

### Author: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)
### Version History ------------------------------------------------------------
# v00, 17.05.2024:    Multifunctionality from average and threshold approach.



### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))

## Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
efp_in <- as.character(read.table("data/efp_version.txt"))
if (savedata) {
  vers_out <- "v00"
  cat(paste0(vers_out, "\n"), file = "data/emf_version.txt")
}



### Utilities ------------------------------------------------------------------
library(readr)      # read data
# library(multifunc)  # multifunctionality
library(ggplot2)    # plotting
library(ggradar)
library(dendextend)
library(patchwork)  # combine plots
library(factoextra)
library(gridExtra)
library(tidyr)      # data manipulation
library(dplyr)
library(purrr)
# library(forcats)
library(glue)
# library(car)        # analysis

## Functions
source("scripts/functions/min_max_norm.R")

## Other
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")
source("scripts/themes/MyThemes.R")



### Data -----------------------------------------------------------------------
## EFPs (functions):
load(glue("data/inter/efps_names_{efp_in}.RData")); efps_names <- efps_names[efps_names != "NEP99"]

## Dataset of EFPs & predictors ----
dat <- read_csv(glue::glue("data/inter/data_efps_clim_{efp_in}.csv"), show_col_types = F) %>% # EFPs + climate
  select(SITE_ID, all_of(efps_names))



### Processing -----------------------------------------------------------------
## Normalize ----
# Important to compare functions in order to 1) build clusters, 2) calculate multifunctionality
# Should be between 0 and 1
dat_norm <- dat %>%
  # group_by(IGBP) %>% # normalize within biomes based on Manning et al., 2018
  mutate(across(.cols = where(is.double), .fns = min_max_norm)) #%>% # ignore warning of Inf output since it already gives NA
  # ungroup()


## Agglomerative cluster analysis ----
dat_mat <- dat_norm %>% select(-SITE_ID, -contains("IGBP")) %>% t()
X <- dist(dat_mat, method = "euclidean")

clust1 <- hclust(X, method = "complete")


## Plot specs ----
line_width <- line_width_medium

common_theme <- theme_bw() + theme_combine


## Elbow plot to determine number of clusters ----
p_elbow <- fviz_nbclust(
  dat_mat, FUN = hcut, method = "wss", k.max = 5
  ) +
  geom_line(aes(group = 1), linewidth = line_width, color = "steelblue") + 
  geom_point(group = 1, size = 5, color = "steelblue") +
  common_theme +
  theme(title = element_blank())
print(p_elbow)

# Save plot
if (savedata) {
  ggplot2::ggsave(filename = glue::glue("results/cluster/Inflection_{vers_out}.jpg"), plot = p_elbow, device = "jpeg",
                  width = 400, height = 300, units = "mm", dpi = 300)
}


## Plot Dendogram ----
p_dendr <- eclust(dat_mat, FUN = "hclust", k = 4, graph = T, hc_metric = "euclidean", hc_method = "complete") %>% 
  fviz_dend(
    clust1,
    k_colors = line_color_plot, lwd = line_width, # color and line width of tree lines
    color_labels_by_k = F, cex = 1, # color and size of labels
    rect = T, rect_border = Four_colorblind, rect_lty = "solid" # boxes around clusters
    ) +
  xlab("Ecosystem Functional Properties") +
  common_theme +
  theme(
    # axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    title = element_blank()
    )
print(p_dendr)

# Save plot
if (savedata) {
  ggplot2::ggsave(filename = glue::glue("results/cluster/Dendrogram_{vers_out}.jpg"), plot = p_dendr, device = "jpeg",
                  width = 400, height = 300, units = "mm", dpi = 300)
}

# clust2 <- clust1 %>%
#   as.dendrogram() #%>%
#   # set("branches_k_color", k = 4) #%>%
#   # set("branches_lwd", c(1.5, 1, 1.5)) #%>%
#   # set("branches_lty", c(1, 1, 3, 1, 1, 2)) #%>%
#   # set("labels_colors") %>%
#   # set("labels_cex", c(.9, 1.2))
# 
# ggplot(clust2) +
#   # theme_bw() +
#   theme_less_facets +
#   theme(
#     # plot.margin = unit(c(1, 1, 1, 1), "cm")
#     panel.spacing = unit(c(1, 1, 1, 1), "cm")
#   )
# 
# 
# pdf("results/cluster/dendogram.pdf", width = 4, height = 3, paper = 'special')
# plot(clust1, cex = 0.6, hang = -1, xlab = "", sub = "")
# rect.hclust(clust1, k = 4, border = Four_colorblind)
# dev.off()

## Combine plots ----
p_hca <- (p_dendr | p_elbow) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = text_size_big))
print(p_hca)

# Save plot
if (savedata) {
  ggplot2::ggsave(filename = glue::glue("results/cluster/hierarchical_clustering_{vers_out}.jpg"), plot = p_hca, device = "jpeg",
                  width = 400, height = 300, units = "mm", dpi = 150)
  ggplot2::ggsave(filename = glue::glue("results/cluster/hierarchical_clustering_{vers_out}.pdf"), plot = p_hca, device = "pdf",
                  width = 400, height = 300, units = "mm", dpi = 600)
}



## Reduce dimensions (average clustered functions) (MANUAL) ----
efps_sub <- c("Cfuns", "WUEfuns", "CUEeco", "Gsmax")
dat_sub <- dat_norm %>% 
  rowwise() %>% 
  mutate(
    Cfuns = mean(c(GPPsat, NEPmax), na.rm = T),
    WUEfuns = mean(c(WUE, uWUE), na.rm = T),
    across(.cols = where(is.double), .fns = ~ if_else(is.nan(.x), NA_real_, .x)) # convert NaN into NA
  ) %>%
  ungroup() %>% # ungroup rowwise
  select(SITE_ID, all_of(efps_sub)) # remove non-averaged functions



### Calculate multifunctionality -----------------------------------------------
dat_emf <- dat_sub


## Average method ----
dat_emf <- dat_emf %>% 
  pivot_longer(cols = all_of(efps_sub), names_to = "EFP_name", values_to = "EFP_value") %>% 
  group_by(SITE_ID) %>% 
  mutate(
    EMFavg = mean(EFP_value, na.rm = T)
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = EFP_name, values_from = EFP_value) %>% 
  glimpse()


## Threshold method ----
threshold <- 0.5 # 50% threshold as in the example in Manning et al., 2018

dat_emf <- dat_emf %>% 
  pivot_longer(cols = all_of(efps_sub), names_to = "EFP_name", values_to = "EFP_value") %>% 
  group_by(SITE_ID) %>% 
  mutate(
    EMFthr = sum(EFP_value > threshold, na.rm = T) / length(efps_sub)
  ) %>% 
  ungroup() %>% 
  pivot_wider(names_from = EFP_name, values_from = EFP_value) %>% 
  glimpse()



## Radar chart method ----
# WARNING: area of radar plot increases quadratically, not linearly (overevaluation of differences).
# Also, radar plots are often criticized as the circular layout is harder to read 
# and choice of ordering might lead to misleading interpretation.

# IDEA: mutlidimensional area inscribed within vertices of all PCA vectors, divided by number of dimensions considered (PCs).
## PCA ----
dat_pca <- dat_norm %>% left_join(readr::read_csv("data/input/igbp.csv", show_col_types = F), by = "SITE_ID") %>% dplyr::filter(IGBP != "CVM") %>% drop_na()

## Run PCA without multiple imputation
pca_result <- FactoMineR::PCA(dat_pca %>% dplyr::select(-SITE_ID, -IGBP), scale.unit = T, ncp = 10, graph = F)
pca1 <- ade4::dudi.pca(dat_pca %>% dplyr::select(-SITE_ID, -IGBP), center = TRUE, scale = TRUE, scannf = FALSE, nf = 10)


## Plot ----
plot_elements_dark <- "gray25"
title_text <- 7; subtitle_text <- 6; normal_text <- 6
fviz_pca_biplot(pca_result,
                axes = c(1, 2),
                col.ind = dat_pca$IGBP, #"grey50",
                # col.ind = NA, #plot_elements_light, #"white",
                geom.ind = "point",
                palette = CatCol_igbp,#'futurama',
                label = "var",
                col.var = plot_elements_dark,
                labelsize = 2,
                repel = TRUE,
                pointshape = 16,
                pointsize = 2,
                alpha.ind = 0.67,
                arrowsize = 0.5) +
  labs(title = "",
       x = "PC1",
       y = "PC2",
       fill = "IGBP") +
  guides(fill = guide_legend(title = "")) +
  theme(title = element_blank(),
        text = element_text(size = normal_text),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = title_text, face = "bold"),
        axis.text = element_text(size = normal_text),
        # plot.margin = unit(c(0, 0, 0, 0), "cm"),
        # legend.position = "none"
        legend.text = element_text(size = subtitle_text),
        legend.key.height = unit(5, "mm"),
        legend.key.width = unit(2, "mm")
  ) +
  NULL

pca_load <- pca_result$var$coord %>% # add loadings
  as_tibble(rownames = "var") %>%
  pivot_longer(cols = !var, names_to = "PC", values_to = "loading") %>% 
  mutate(PC = paste0("PC", stringr::str_sub(PC, start = 5)))

dat_emf %>% 
  dplyr::slice_sample(n = 3) %>% 
  select(SITE_ID, all_of(efps_sub)) %>% 
  drop_na() %>% 
  ggradar(#values.radar = c(0, 0.5, 1),
          )



### Vector names of variables --------------------------------------------------
## Define names of Ecosystem MultiFunctionality
emf_names <- dat_emf %>% dplyr::select(-SITE_ID, -contains("IGBP"), -contains("Rao"), -any_of(efps_names), -any_of(efps_sub)) %>% names()
if (savedata) {
  ## Save
  save(emf_names, file = glue::glue("data/inter/emf_names_{vers_out}.RData"))
}



### Save data ------------------------------------------------------------------
if (savedata) {
  dat_emf <- dat_emf %>% select(SITE_ID, all_of(emf_names))
  write_csv(dat_emf, glue::glue("data/inter/data_emf_{vers_out}.csv"))
}



### End ------------------------------------------------------------------------