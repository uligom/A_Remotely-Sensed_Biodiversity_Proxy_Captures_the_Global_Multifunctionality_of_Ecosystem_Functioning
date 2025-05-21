#### EXPLORE RAOQ
# RaoQ estimates from Javier Pacheco-Labrador

### Author: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))



### Options --------------------------------------------------------------------
# Data settings
efp_in <- as.character(read.table("data/efp_version.txt"))

savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved

if (savedata) {
  eval_file <- glue::glue("results/analysis_evaluation/evaluation_raoq_extraction.txt")
  txt <- "Initializing analysis..."; print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = F)}
}


### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)      # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)    # tidy plot
library(glue)       # build regex strings
library(lubridate)  # dates
library(readr)      # read tables
library(scales)     # scale functions for visualization
library(stringr)    # string manipulation
library(terra)      # raster data
library(tidyr)

## Functions
source("scripts/functions/plot_timeseries.R")
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### Data -----------------------------------------------------------------------
## IGBP
igbp <- read_csv("data/input/igbp.csv", show_col_types = F)
clim <- read_csv(glue::glue("data/inter/clim_{efp_in}.csv"), show_col_types = F)

## Folder paths
tif_folder <- "//minerva/BGI/work_2/ugomar/S2_L2A_fluxnet_incl_baresoil/"
raoq_folder <- "//minerva/BGI/work_2/ugomar/S2_L2A_fluxnet_RaoQ/S2_L2A_fluxnet_incl_baresoil/"


## RaoQ from single scenes: Sentinel 2 band files
bands_raoq_files <- list.files(raoq_folder, pattern = ".csv") # available files from Javier

## Sites with available RaoQ data files
sites <- stringr::str_extract(bands_raoq_files, "[:graph:]+(?=_S2_2A_bands.csv)") %>% na.omit() # extract site names
# Check missing sites
sites_upload <- read_csv("data/inter/site_list_all.csv", show_col_types = F) %>% pull(SITE_ID)
sites_missing <- setdiff(sites_upload, sites)

# Check if downloaded data is present and where
if (length(sites_missing) > 0) {
  zip_directories <- list.files(tif_folder, pattern = ".zip")
  for (zz in 1:length(zip_directories)) {
    zip_files <- unzip(paste0(tif_folder, zip_directories[zz]), list = T)
    for (ss in 1:length(sites_missing)) {
      if (zz == 1 & ss == 1) {matched <- tibble()}
      if (any(str_detect(zip_files$Name, pattern = sites_missing[ss]))) {
        matched <- matched %>% 
          bind_rows(
            tibble(
              zip_dir = zip_directories[zz],
              zip_file = zip_files$Name[str_which(zip_files$Name, pattern = sites_missing[ss])]
            )
          )
      }
    }
  }
  matched
}


## Extract RaoQ for single bands for all sites
dat_inclBareSoil <- tibble() # initialize
for (ii in 1:length(sites)) {
  ## RaoQ from bands for all scenes ----
  dat_site <- read_delim(glue::glue("{raoq_folder}{bands_raoq_files[ii]}"), delim = ";",
                         show_col_types = F) %>% 
    # drop_na(Year)
    bind_cols(
      tibble(
        DATE = terra::rast(x = glue("//minerva/BGI/work_2/ugomar/S2_L2A_fluxnet_incl_baresoil/S2_fluxsites_cutouts/{sites[ii]}_S2_2A_bandB4.tif"))@ptr[["names"]] %>% 
          str_extract(pattern = "[:digit:]{8}") %>%
          as.Date(format = "%Y%m%d")
      )
    ) %>% 
    relocate(DATE, .before = f_valid_samples) %>% 
    select(-Year, -DoY, -`...1`)
  
  if (nrow(dat_site) == 0) { # if input data is empty
    warning(glue::glue("No RaoQ metric could be extracted for site '{sites[ii]}': no input data."))
    next
  }
  
  # dat_site <- dat_site %>% 
  #   rename(YEAR = Year, DOY = DoY) %>% 
  #   mutate(YEAR = as.integer(YEAR),
  #          DATE = as.Date(DOY - 1, origin = paste0(YEAR, "-01-01"))
  #          ) %>% # subtract 1 from DOY because R uses a 0 base index, otherwise DOY = 1 will be Jan. 2.
  #   relocate(DATE, .before = YEAR)
  
  # ## Plots for single sites ----
  # ## Point specs
  # point_border_color <- "white"
  # point_fill_color <- "#EE4500"
  # point_shape <- 21
  # small_point_size <- 3
  # # main_point_size <- 6
  # 
  # raoq <- rlang::sym("RaoQ_NIRv_S2_median") # metric to plot: "RaoQ_S2_median", "RaoQ_NDVI_S2_median", or "RaoQ_NIRv_S2_median"
  # 
  # p_timeseries <- dat_site %>% 
  #   mutate(f_valid_samples = 0.2 * round(f_valid_samples / 0.2)) %>% 
  #   drop_na(!!raoq) %>% 
  #   ggplot(aes(x = DATE, y = !!raoq)) +
  #   guides(color = guide_legend(title = "Reference value")) +
  #   geom_line(color = "gray50", linewidth = 0.75, na.rm = T) +
  #   geom_point(aes(fill = f_valid_samples), color = point_border_color, shape = point_shape, size = small_point_size, na.rm = T) +
  #   scale_fill_viridis_c(option = "viridis", direction = -1,
  #                        breaks = c(0.3, 0.5, 0.7, 0.9), limits = c(0.3, 0.9), oob = squish, # set range and squish out-of-bounds values to range
  #                        na.value = "#E2A2BE") +
  #   guides(fill = guide_legend(title = "Fraction of valid pixels")) +
  #   labs(title = glue::glue("Site {sites[ii]}")) + xlab("") +
  #   theme_bw() +
  #   # theme(legend.position = "none") +
  #   NULL
  # 
  # 
  # ## Save plots ----
  # if (savedata) {
  #   ggsave(filename = glue::glue("results/timeseries/raoq/{raoq}_{sites[ii]}.jpg"),
  #          plot = p_timeseries, device = "jpeg",
  #          width = 508, height = 285.75, units = "mm", dpi = 100)
  # }
  # 
  # 
  ## Merge data ----
  dat_inclBareSoil <- bind_rows(
    dat_inclBareSoil,
    bind_cols(
      SITE_ID = sites[ii],
      dat_site
    )
  )
} # end ii for loop
rm(dat_site) # clean memory



# ## RaoQ without Bare Soil ----
# ## Folder paths
# tif_folder <- "//minerva/BGI/work_2/ugomar/S2_L2A_fluxnet_no_baresoil/"
# raoq_folder <- "//minerva/BGI/work_2/ugomar/S2_L2A_fluxnet_RaoQ/S2_L2A_fluxnet_no_baresoil/"
# 
# 
# ## RaoQ from single scenes: Sentinel 2 band files
# bands_raoq_files <- list.files(raoq_folder, pattern = ".csv") # available files from Javier
# 
# ## Sites with available RaoQ data files
# sites <- stringr::str_extract(bands_raoq_files, "[:graph:]+(?=_S2_2A_bands.csv)") %>% na.omit() # extract site names
# # Check missing sites
# sites_upload <- read_csv("data/inter/site_list_all.csv", show_col_types = F) %>% pull(SITE_ID)
# sites_missing <- setdiff(sites_upload, sites)
# 
# # Check if downloaded data is present and where
# if (length(sites_missing) > 0) {
#   zip_directories <- list.files(tif_folder, pattern = ".zip")
#   for (zz in 1:length(zip_directories)) {
#     zip_files <- unzip(paste0(tif_folder, zip_directories[zz]), list = T)
#     for (ss in 1:length(sites_missing)) {
#       if (zz == 1 & ss == 1) {matched <- tibble()}
#       if (any(str_detect(zip_files$Name, pattern = sites_missing[ss]))) {
#         matched <- matched %>% 
#           bind_rows(
#             tibble(
#               zip_dir = zip_directories[zz],
#               zip_file = zip_files$Name[str_which(zip_files$Name, pattern = sites_missing[ss])]
#             )
#           )
#       }
#     }
#   }
#   matched
# }
# 
# 
# ## Extract RaoQ for single bands for all sites
# dat_noBareSoil <- tibble() # initialize
# for (ii in 1:length(sites)) {
#   ## RaoQ from bands for all scenes ----
#   dat_site <- read_delim(glue::glue("{raoq_folder}{bands_raoq_files[ii]}"), delim = ";",
#                          show_col_types = F) %>% 
#     bind_cols(
#       tibble(
#         DATE = terra::rast(x = glue("//minerva/BGI/work_2/ugomar/S2_L2A_fluxnet_incl_baresoil/S2_fluxsites_cutouts/{sites[ii]}_S2_2A_bandB4.tif"))@ptr[["names"]] %>% 
#           str_extract(pattern = "[:digit:]{8}") %>%
#           as.Date(format = "%Y%m%d")
#       )
#     ) %>% 
#     relocate(DATE, .before = f_valid_samples) %>% 
#     select(-Year, -DoY, -`...1`)
#   
#   if (nrow(dat_site) == 0) { # if input data is empty
#     warning(glue::glue("No RaoQ metric could be extracted for site '{sites[ii]}': no input data."))
#     next
#   }
#   
#   # dat_site <- dat_site %>% 
#   #   rename(YEAR = Year, DOY = DoY) %>% 
#   #   mutate(YEAR = as.integer(YEAR),
#   #          DATE = as.Date(DOY - 1, origin = paste0(YEAR, "-01-01"))
#   #   ) %>% # subtract 1 from DOY because R uses a 0 base index, otherwise DOY = 1 will be Jan. 2.
#   #   relocate(DATE, .before = YEAR)
#   
#   
#   ## Merge data ----
#   dat_noBareSoil <- bind_rows(
#     dat_noBareSoil,
#     bind_cols(
#       SITE_ID = sites[ii],
#       dat_site
#     )
#   )
# } # end ii for loop
# 
# rm(dat_site) # clean memory
# 
# 
# 
# ## Plot RaoQ with/without Bare Soil ----
# dat_plot <- full_join(
#   dat_noBareSoil, dat_inclBareSoil,
#   by = c("SITE_ID", "DATE"),
#   suffix = c("_no", "_incl")
# ) %>% 
#   glimpse()
# 
# ## All data points:
# dat_plot %>% 
#   ggplot(aes(x = RaoQ_NIRv_S2_median_no, y = RaoQ_NIRv_S2_median_incl)) +
#   geom_point(na.rm = T) +
#   geom_smooth(method = "lm", na.rm = T) +
#   geom_abline(slope = 1, color = "red2") +
#   theme_bw()
# 
# ## Density/kernel regions:
# dat_plot %>% 
#   ggplot(aes(x = RaoQ_NIRv_S2_median_no, y = RaoQ_NIRv_S2_median_incl)) +
#   geom_density_2d_filled(show.legend = F, na.rm = T) +
#   geom_abline(slope = 1, color = "red2") +
#   # xlim(0, max(dat_plot$RaoQ_NIRv_S2_median_no, na.rm = T)) +
#   # ylim(0, max(dat_plot$RaoQ_NIRv_S2_median_incl, na.rm = T)) +
#   theme_bw()
# 
# ## Aggregate data points to sites
# dat_plot <- dat_plot %>% 
#   group_by(SITE_ID) %>% 
#   summarise(
#     RaoQ_NIRv_S2_median_no = mean(RaoQ_NIRv_S2_median_no, na.rm = T),
#     RaoQ_NIRv_S2_median_incl = mean(RaoQ_NIRv_S2_median_incl, na.rm = T)
#   ) %>% 
#   ungroup() %>% 
#   left_join(igbp, by = "SITE_ID") %>%
#   left_join(clim, by = "SITE_ID") %>%
#   relocate(IGBP, .after = SITE_ID) %>% 
#   glimpse()
# 
# p_bias <- dat_plot %>% 
#   ggplot(aes(x = RaoQ_NIRv_S2_median_no, y = RaoQ_NIRv_S2_median_incl,
#              color = IGBP)) +
#   # geom_density_2d(aes(x = RaoQ_NIRv_S2_median_no, y = RaoQ_NIRv_S2_median_incl), color = text_color_background, 
#   #                 inherit.aes = F, show.legend = F, na.rm = T) +
#   geom_point(aes(size = VPD), alpha = alpha_medium, na.rm = T) +
#   ggrepel::geom_text_repel(aes(label = SITE_ID), size = point_size_small,
#                             show.legend = F, na.rm = T) + # add labels of site names
#   scale_color_manual(values = CatCol_igbp) +
#   geom_abline(slope = 1, color = "red2", linewidth = line_width_thin) +
#   xlab(expression("Only-vegetation-RaoQ"[NIRv])) +
#   ylab(expression("Bare+vegetation-RaoQ"[NIRv])) +
#   guides(color = guide_legend(title = "IGBP class", override.aes = list(size = point_size_medium_small))) +
#   theme_bw()
# print(p_bias)
# 
# ## Save
# if (savedata) {
#   # RaoQ bias plot
#   ggplot2::ggsave(filename = glue::glue("bias_RaoQ_NIRv_with-without_BareSoil.jpg"), plot = p_bias, device = "jpeg",
#                   path = "results/scatterplots/", width = 508, height = 285.75, units = "mm", dpi = 150) # 1920 x  1080 px resolution (16:9)
# }
# 
# 

### Plot -----------------------------------------------------------------------
## Data for plotting ----
dat_plot <- dat_inclBareSoil %>% mutate(f_valid_samples = 0.2 * round(f_valid_samples / 0.2))


## Time series of RaoQ metrics ----
dat_plot %>% plot_timeseries(x = "DATE", y = "RaoQ_S2_median", color = "f_valid_samples", facet = "SITE_ID", savepath = "results/timeseries/")

dat_plot %>% plot_timeseries(x = "DATE", y = "RaoQ_NDVI_S2_median", color = "f_valid_samples", facet = "SITE_ID", savepath = "results/timeseries/")

dat_plot %>% plot_timeseries(x = "DATE", y = "RaoQ_NIRv_S2_median", color = "f_valid_samples", facet = "SITE_ID", savepath = "results/timeseries/")




### Calculate/rename VIs -------------------------------------------------------
dat <- dat_inclBareSoil %>%
  select(-contains("NDVI_median"), -contains("NIRv_median"), 
         -contains("NDVI_std"), -contains("NIRv_std")) %>% 
  rename(NDVI_median = NDVI_S2_median, NDVI_std = NDVI_S2_std,
         NIRv_median = NIRv_S2_median, NIRv_std = NIRv_S2_std)
  # mutate(
  #   NDVI_median = (B8_median - B4_median) / (B8_median + B4_median), # (NIR - red) / (NIR + red)
  #   NIRv_median = B8_median / NDVI_median # NIR / NDVI
  # )



### Plot VIs -------------------------------------------------------------------
## Plot NDVI
p_ndvi <- dat %>%
  mutate(f_valid_samples = 0.2 * round(f_valid_samples / 0.2)) %>% # round values
  plot_timeseries("DATE", "NDVI_median", color = "f_valid_samples", facet = "SITE_ID") +
  # ylim(c(0.5, 1.0)) +
  scale_color_viridis_c(option = "viridis", direction = -1, alpha = 0.7,
                       breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0.5, 1.0), oob = squish, # set range and squish out-of-bounds values to range
                       na.value = na_color) +
  guides(color = guide_legend(title = "Fraction of valid pixels", override.aes = list(size = point_size_big))) +
  NULL
p_ndvi

## Plot NIRv
p_nirv <- dat %>%
  mutate(f_valid_samples = 0.2 * round(f_valid_samples / 0.2)) %>% 
  plot_timeseries("DATE", "NIRv_median", color = "f_valid_samples", facet = "SITE_ID") +
  # ylim(c(0.5, 1.0)) +
  scale_color_viridis_c(option = "viridis", direction = -1, alpha = 0.5,
                        breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0.5, 1.0), oob = squish, # set range and squish out-of-bounds values to range
                        na.value = na_color) +
  guides(color = guide_legend(title = "Fraction of valid pixels", override.aes = list(size = point_size_big))) +
  NULL
p_nirv



### FILTER data ----------------------------------------------------------------
# NDVIquants <- quantile(dat$NDVI_median, probs = c(seq(0.5:1, by = 0.05)), na.rm = T)
q_thresh <- 0.9
f_thresh <- 0.5

## Number of sites before filtering
sites_unfilt <- dat %>% drop_na(RaoQ_S2_median, RaoQ_NDVI_S2_median, RaoQ_NIRv_S2_median) %>%
  select(SITE_ID) %>% unique()

txt <- glue::glue("{nrow(sites_unfilt)} NEON sites with valid RaoQ estimates BEFORE filtering.")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}

## Filtering
txt <- glue::glue("==> Filtering out scenes below {q_thresh*100}th quantile NDVI threshold, and with less than {f_thresh*100}% valid pixels.")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}

dat_filt <- dat %>% 
  group_by(SITE_ID) %>% 
  mutate(NDVIthresh = quantile(NDVI_median, probs = q_thresh, na.rm = T), .after = NDVI_median) %>% 
  ungroup() %>% 
  dplyr::filter(f_valid_samples > f_thresh) %>% # filter on % of valid pixels
  dplyr::filter(NDVI_median >= NDVIthresh) # filter on NDVI threshold


## Number of sites after filtering
sites_filt <- dat_filt %>% drop_na(RaoQ_S2_median, RaoQ_NDVI_S2_median, RaoQ_NIRv_S2_median) %>%
  select(SITE_ID) %>% unique()

txt <- glue::glue("{nrow(sites_filt)} NEON sites with valid RaoQ estimates AFTER filtering.")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
txt <- paste0("--> Excluded sites: ", setdiff(sites_unfilt, sites_filt) %>% pull(SITE_ID) %>% paste(collapse = ", "))
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}



### CALCULATE aggregated site RaoQ and NDVImax ---------------------------------
dat_agg <- dat_filt %>% 
  dplyr::select(-DATE, -NDVIthresh) %>% 
  group_by(SITE_ID) %>% 
  mutate(NDVImax = quantile(NDVI_median, 0.95, na.rm = T), # calculate NDVImax
         NIRvmax = quantile(NIRv_median, 0.95, na.rm = T) # calculate NIRvmax
         ) %>%
  summarise(across(.cols = c(where(is.double), -NDVImax, -NIRvmax), ~ mean(.x, na.rm = T)),
            NDVImax = unique(NDVImax),
            NIRvmax = unique(NIRvmax)
            ) %>% # average every variable
  ungroup() %>% 
  relocate(NDVImax, .after = NDVI_median) %>% 
  relocate(NIRvmax, .after = NIRv_median)



### Rename RaoQ variables ----
dat_agg <- dat_agg %>% 
  dplyr::rename(
    Rao_Q_S2 = RaoQ_S2_median,
    Rao_Q_NDVI = RaoQ_NDVI_S2_median,
    Rao_Q_NIRv = RaoQ_NIRv_S2_median
  ) %>% 
  glimpse()


### Calculate correlations -----------------------------------------------------
temp <- dat_agg %>% 
  dplyr::select(NIRv_median, Rao_Q_NIRv) %>%
  drop_na()

cor_coeff <- cor(temp$NIRv_median, temp$Rao_Q_NIRv, method = "kendall")
# Kendall more robust with small sample sizes or outliers
cor_coeff

## Save correlation coefficient
if (savedata) {
  cat(
    glue::glue("Kendall’s tau = {round(cor_coeff, digits = 2)} over {nrow(temp)} sites"),
    file = "results/correlation/RaoQNIRv_vs_NIRvmedian.txt"
    )
}



### Save -----------------------------------------------------------------------
if (savedata) {
  # Band data
  write_csv(dat, "data/inter/raoq/S2_L2A_bands_allsites.csv")
  
  # New aggregated RaoQ estimates
  write_csv(dat_agg, "data/inter/raoq/raoQ_S2_L2A_filtered_averaged.csv")
  
  # NDVI timeseries
  ggplot2::ggsave(filename = glue::glue("NDVI_FromBandsMedians.jpg"), plot = p_ndvi, device = "jpeg",
                  path = "results/timeseries/", width = 508, height = 285.75, units = "mm", dpi = 150) # 1920 x  1080 px resolution (16:9)
  # NIRv timeseries
  ggplot2::ggsave(filename = glue::glue("NIRv_FromBandsMedians.jpg"), plot = p_nirv, device = "jpeg",
                  path = "results/timeseries/", width = 508, height = 285.75, units = "mm", dpi = 150) # 1920 x  1080 px resolution (16:9)
}
### End ------------------------------------------------------------------------