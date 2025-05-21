#### PLOT COEFFICINETS OF MULTIMODEL INFERENCE

### Authors: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)
### Version History
### Script settings ------------------------------------------------------------
# Clear environment
# rm(list = ls(all = TRUE))

library(tictoc)
tic() # measure run time

# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved

efp_in <- as.character(read.table("data/efp_version.txt"))
# iav_in <- as.character(read.table("data/iav_version.txt"))
dat_in <- as.character(read.table("data/data_version.txt"))
vers <- as.character(read.table("data/mumin_analysis_version.txt"))
# v01.030, 08.05.2024:  Main analyses.
# v01b.030,14.05.2024:  Testing measured LAImax in models.
# v02.030, 21.05.2024:  Testing NIRv_median together with RaoQ_NIRv.
# v03.030, 27.05.2024:  Testing soil variables.
# v04.030, 27.05.2024:  Testing only IAV climate variables.



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)        # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)      # tidy plots
library(patchwork)    # combine plots
library(RColorBrewer) # plot color functionalities
library(readr)        # read table format files
library(scales)       # modify scales of axes
library(stringr)      # string manipulation
library(tidyr)        # clean and reshape tidy data

## Other
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### Process --------------------------------------------------------------------
for (eee in 1:2) {
  ## Select response variables ----
  if (eee == 1) {response <- "efp"} else if (eee == 2) {response <- "emf"}
  # "efp" = EFPs calculated on full site timeseries
  # # "iav" = stability of EFPs (not supported)
  # "emf" = multifunctionality of EFPs
  
  
  ## Select subset of predictors ----
  subx <- "all"
  # "no"      = exclude all biodiversity variables
  # "all"     = include all biodiversity predictors


  ## Select RaoQ input ----
  raoq_in <- "nirv"
  # "bands" = raoQ from all bands
  # "ndvi" = raoQ from NDVI
  # "nirv" = raoQ from NIRv
  
  
  
  ### More output settings -----------------------------------------------------
  vers_in <- glue::glue("{response}_{subx}_{raoq_in}_{vers}")
  vers_out <- paste0(vers_in, "_manuscript05")
  
  
  
  ### Data ---------------------------------------------------------------------
  dat <- read_csv(glue::glue("results/multimodel_inference/prediction_and_relaimpo_{vers_in}.csv"), show_col_types = F)
  
  
  
  ### Vector names -------------------------------------------------------------
  ## Meteorology
  if (response == "efp") { # analysis on full-timeseries EFPs
    load(file = glue::glue("data/inter/clim_names_{efp_in}.RData"));  clim_names <- clim_names[clim_names %in% dat$variable]
  # } else if (response == "iav") { # analysis on stability of EFPs
  #   load(file = glue::glue("data/inter/clim_names_{efp_in}.RData")) # mean Meteorology
  #   load(file = glue::glue("data/inter/clim_iav_names_{iav_in}.RData")) # Meteorology variability
  #   clim_names <- c(clim_names[clim_names %in% dat$variable], clim_iav_names[clim_iav_names %in% dat$variable]);
  } else if (response == "emf") { # analysis on multifunctionality of EFPs
    load(file = glue::glue("data/inter/clim_names_{efp_in}.RData"));  clim_names <- clim_names[clim_names %in% dat$variable]
  }
  
  ## Structure
  load(file = glue::glue("data/inter/struct_names_{efp_in}.RData"));  struct_names <- struct_names[struct_names %in% dat$variable]
  
  
  
  ## Omit NAs ----
  dat <- dat %>% drop_na()
  
  
  # ## Select significant predictors ----
  # alpha <- 0.05
  # 
  # dat <- dat %>% dplyr::filter(p_val < alpha) # exclude non-significant predictors for plotting
  # 
  # 
  ## Number of observations ----
  # Order of n_obs same as EFPs
  n_obs <- dat %>%
    drop_na() %>%
    select(prediction, n) %>% 
    unique() %>% 
    pull(n)
  
  
  
  ### Plot ---------------------------------------------------------------------
  ## Labels and options ----
  ## Add color labels
  if (any(c("CLAY", "ORCDRC", "PHIHOX") %in% unique(dat$variable))) {
    accessible_palette <- setNames(Four_colorblind, c("Biodiversity proxy", "Mean meteorology", "Structural properties", "Soil properties"))
  } else {
    accessible_palette <- setNames(Three_colorblind2, c("Biodiversity proxy", "Mean meteorology", "Structural properties")) 
  }
  
  ## Add predictor labels
  dat <- dat %>% 
    mutate(var_type = case_when(
      variable %in% clim_names & !variable %in% c("CLAY", "ORCDRC", "PHIHOX") ~ "Mean meteorology",
      variable %in% c("CLAY", "ORCDRC", "PHIHOX") ~ "Soil properties",
      variable %in% struct_names ~ "Structural properties",
      variable == "NIRv_median" ~ "Structural properties",
      str_detect(variable, "Rao") ~ "Biodiversity proxy",
      T ~ "Other"
    )
    )
  
  
  ## Add levels for y axis order
  # variable as factor with levels to define order on y axis
  var_names <- dat %>% dplyr::arrange(desc(var_type), desc(variable)) %>% pull(variable) %>% unique()
  y_names <- dat %>% arrange(prediction) %>% pull(prediction) %>% unique()
  if (response == "efp") {
    if(length(y_names) == 5) {
      y_labels <- c(expression("CUE"[eco]), expression("GPP"[sat]), expression("Gs"[max]), expression("NEP"[max]), expression("WUE"))#, " [(n == ", n_obs, ")]")
    } else if (length(y_names) == 6) {
      y_labels <- c(expression("CUE"[eco]), expression("GPP"[sat]), expression("Gs"[max]), expression("NEP"[max]), expression("WUE"), expression("uWUE"))
    }
  } else if (response == "iav") {
    if (iav_in %in% c("v00", "v01", "v04", "v05")) { # CV = coefficient of variation
      if (length(y_names) == 5) {
        y_labels <- c(expression("CUEeco"[cv]), expression("GPPsat"[cv]), expression("Gsmax"[cv]), expression("NEPmax"[cv]), expression("WUE"[cv]))#, " [(n == ", n_obs, ")]")
      } else if (length(y_names) == 6) {
        y_labels <- c(expression("CUEeco"[cv]), expression("GPPsat"[cv]), expression("Gsmax"[cv]), expression("NEPmax"[cv]), expression("WUE"[cv]), expression("uWUE"[cv]))
      }
    } else if (iav_in %in% c("v02", "v03", "v06")) { # STD = standard deviation
      if (length(y_names) == 5) {
        y_labels <- c(expression("CUEeco"[std]), expression("GPPsat"[std]), expression("Gsmax"[std]), expression("NEPmax"[std]), expression("WUE"[std]))#, " [(n == ", n_obs, ")]")
      } else if (length(y_names) == 6) {
        y_labels <- c(expression("CUEeco"[std]), expression("GPPsat"[std]), expression("Gsmax"[std]), expression("NEPmax"[std]), expression("WUE"[std]), expression("uWUE"[std]))
      }
    } else if (iav_in %in% c("v07", "v08")) { # ES = ecosystem stability
      if (length(y_names) == 5) {
        y_labels <- c(expression("CUEeco"[es]), expression("GPPsat"[es]), expression("Gsmax"[es]), expression("NEPmax"[es]), expression("WUE"[es]))#, " [(n == ", n_obs, ")]")
      } else if (length(y_names) == 6) {
        y_labels <- c(expression("CUEeco"[es]), expression("GPPsat"[es]), expression("Gsmax"[es]), expression("NEPmax"[es]), expression("WUE"[es]), expression("uWUE"[es]))
      }
    }
  } else if (response == "emf") {
    if(length(y_names) == 2) {
      y_labels <- c(expression("EMF"[average]), expression("EMF"[threshold]))#, " [(n == ", n_obs, ")]")
    } else if (length(y_names) == 3) {
      y_labels <- c(expression("EMF"[average]), expression("EMF"[threshold]), expression("EMF"[radar]))#, " [(n == ", n_obs, ")]")
    }
  }
  
  nice_names <- var_names %>% str_replace_all("_", " ")
  
  dat <- dat %>% 
    mutate(
      variable = factor(
        variable,
        levels = var_names,
        labels = nice_names
      ),
      prediction = factor(
        prediction,
        levels = y_names,
        labels = y_labels
      ),
      var_type = factor(
        var_type,
        levels = c("Biodiversity proxy", "Mean meteorology", "Structural properties", "Soil properties")
      ),
      rel_cat = case_when( # add relative importance categories
        rel_importance >= 0.0 & rel_importance < 0.1 ~ 0.00,
        rel_importance >= 0.1 & rel_importance < 0.2 ~ 0.25,
        rel_importance >= 0.2 & rel_importance < 0.3 ~ 0.50,
        rel_importance >= 0.3 & rel_importance < 0.4 ~ 0.75,
        rel_importance >= 0.4 & rel_importance < 1.0 ~ 1.00
      ) %>% factor(
        levels = c(0.00, 0.25, 0.50, 0.75, 1.00),
        labels = c("0%-10%", "10%-20%", "20%-30%", "30%-40%", "40%-100%")
      ),
    ) %>%
    arrange(prediction, var_type, variable) %>% 
    glimpse()
  
  
  # x limits
  x_lim <- max(abs(dat %>% mutate(val_std = if_else(estimate > 0, true = estimate + std_error, false = estimate - std_error)) %>% pull(val_std)), na.rm = T) * 1.1 # identify max coefficient across analyses and round, with a buffer
  # # x_lim <- 0.05 * ceiling(max(abs(dat$estimate), na.rm = T) / 0.05) * 1.1 # identify max coefficient across analyses and round, with a buffer
  # x_breaks <- c(-0.05 * round(x_lim / 0.05), 0, 0.05 * round(x_lim / 0.05))
  # x_labels <- c(format(-0.05 * round(x_lim / 0.05), nsmall = 2), format(-0.05 * round(x_lim / 0.05) / 2, nsmall = 2), "0.00", format(0.05 * round(x_lim / 0.05) / 2, nsmall = 2), format(0.05 * round(x_lim / 0.05), nsmall = 2))
  
  # plot specifications
  line_width <- line_width_thick
  point_size <- point_size_medium
  
  
  ## Plot model coefficients ----
  p_effects <- list() # initialize list of plots
  for (pp in 1:length(unique(dat$prediction))) {
    # data (pp)
    dat_pp <- dat %>% dplyr::filter(prediction == unique(dat$prediction)[pp])
    
    # caption (pp)
    labelcap <- bquote(R^2 ~ "=" ~ .(sprintf("%.1f", signif(unique(dat_pp$R2), 3) * 100)) ~ "%" ~
                         "  RMSE =" ~ .(sprintf("%.1f", signif(unique(dat_pp$RMSE), 2))) ~
                         "  n =" ~ .(unique(dat_pp$n))
    )
    
    p_effects[[pp]] <- dat_pp %>% 
      ggplot(aes(x = estimate, y = variable)) +
      geom_vline(xintercept = 0, color = text_color_background) + # 0 line
      # geom_point( ### debug with shape check ###
      #   aes(color = var_type, shape = rel_cat),
      #   alpha = 1, size = 12, stroke = line_width, na.rm = T, show.legend = T
      # ) +
      geom_errorbarh( # draw errorbars without transparency
        aes(color = var_type, xmin = estimate - std_error, xmax = estimate + std_error),
        alpha = 1, height = 0, linewidth = line_width, na.rm = T, show.legend = T
      ) +
      geom_point( # add white fill, full-color stroke points to cover errorbars in the central region
        aes(color = var_type),
        alpha = 1, fill = "white",
        stroke = line_width, size = point_size, shape = 21, na.rm = T, show.legend = T
      ) +
      geom_point( # draw round filled shape with transparency
        aes(alpha = rel_cat, color = var_type),
        size = point_size, shape = 19, na.rm = T, show.legend = T
      ) +
      geom_text( # add significance asterisks
        aes(color = var_type, label = ifelse(p_val < 0.05, "*", NA)),
        nudge_x = x_lim * 0.1, nudge_y = 0.1,
        size = point_size * 1.5, na.rm = T, show.legend = F
      ) +
      scale_alpha_discrete(
        # breaks = c(0.05, 0.3, 0.55), labels = c("Low", "Medium", "High"), # display alpha categories
        range = c(0, 1), # display transparency range
        # limits = c(0.04, 0.56), oob = squish # replaces out of bounds values with the nearest limit
        drop = F
      ) +
      scale_discrete_manual( # custom colors
        aesthetics = "color",
        values = accessible_palette,
        na.translate = F
      ) +
      # discrete_scale( # custom shape to check that everything is working as intended
      #   aesthetics = "shape",
      #   scale_name = "rel_cat",
      #   palette = manual_pal(c(25, 23, 24)),
      #   drop = F
      # ) +
      scale_x_continuous(
        breaks = waiver(),
        # labels = x_labels,
        limits = c(-x_lim, x_lim), # define same x axis limits for all subplots
        n.breaks = 5 # "algorithm may choose a slightly different number to ensure nice break labels" <-- do what you want then #!*@$!!
      ) +
      # xlim(c(-x_lim, x_lim)) +
      xlab("Effect coefficient") +
      labs(
        title = y_labels[pp],
        caption = labelcap
      ) +
      guides(
        # shape = guide_legend(title = "Relaimpo check", override.aes = list(stroke = line_width)),
        alpha = guide_legend(title = "Relative importance", override.aes = list(size = point_size)),
        color = guide_legend(title = "Predictor type", override.aes = list(size = point_size))
      ) + # legend titles
      theme_bw() +
      theme_combine +
      theme(
        axis.title.y = element_blank(), # remove axes titles
        legend.background = element_rect(fill = "transparent"),
        plot.caption = element_text(color = text_color_background), # color of caption label
        plot.margin = unit(c(0, 10, 0, 0), "mm")
      ) +
      NULL
    
    # geom_pointrange(
    #   aes(alpha = rel_importance, color = var_type, xmin = estimate - std_error, xmax = estimate + std_error),
    #   size = point_size, shape = 19, linewidth = line_width, fatten = 1, na.rm = T, show.legend = T
    #   ) +
    # size = 1.5 in legend guides for geom_pointrange()
    # geom_point( # add circle without transparency
    #   aes(color = var_type),
    #   alpha = 1, stroke = line_width, size = point_size, shape = 1, na.rm = T, show.legend = F
    #   ) +
    # scale_x_continuous(
    #   breaks = waiver(),
    #   # labels = x_labels,
    #   limits = c(-x_lim, x_lim), # define same x axis limits for all subplots
    #   n.breaks = 5 # "algorithm may choose a slightly different number to ensure nice break labels" <-- do what you want then #!*@$!!
    #   ) +
    
    if (pp != length(unique(dat$prediction))) {
      p_effects[[pp]] <- p_effects[[pp]] + theme(legend.position = "none")
    }
  }
  
  
  ## Combine ----
  if (response %in% c("efp", "iav")) {
    p_effects <- wrap_plots(p_effects, nrow = 2) +
      plot_layout(guides = 'collect') + plot_annotation(tag_levels = "a")
  } else if (response == "emf") {
    p_effects <- wrap_plots(p_effects, ncol = 2) +
      plot_layout(guides = 'collect') + plot_annotation(tag_levels = "a")
  }
  p_effects
  
  
  
  ### Save ---------------------------------------------------------------------
  if (savedata) {
    scal <- 1
    width <- 508
    height <- 285.75 #width * 6.66 / 31.02
    
    # multimodel effects plot
    ggplot2::ggsave(filename = glue::glue("results/multimodel_inference/mumin_coefficients_{vers_out}.jpg"), plot = p_effects, device = "jpeg",
                    width = width, height = height, units = "mm", dpi = 150 * scal)
    
    # # transparent png
    # ggplot2::ggsave(filename = glue::glue("results/multimodel_inference/mumin_coefficients_{vers_out}.png"), plot = p_effects, device = "png",
    #                 bg = "transparent", width = width, height = height, units = "mm", dpi = 300 * scal)
    
    # if (subx == "main") {
    #   # cross validation effects plot
    #   ggplot2::ggsave(filename = glue::glue("results/multimodel_inference/crossval_coefficients_{vers_out}.jpg"), plot = p_crossval, device = "jpeg",
    #                   width = width, height = height, units = "mm", dpi = 300 * scal)
    # }
  }
  
  

} # end loop for type of predicted variables (EFPs vs EMFs)



### End ------------------------------------------------------------------------