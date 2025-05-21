#### PLOT YEARS AFTER 2016

### Authors: Ulisse Gomarasca
### Script settings ------------------------------------------------------------
# Clear environment
# rm(list = ls(all = TRUE))

library(tictoc)
tic() # measure run time

# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved

efp_in <- as.character(read.table("data/efp_version.txt"))
iav_in <- as.character(read.table("data/iav_version.txt"))
dat_in <- as.character(read.table("data/data_version.txt"))
vers <- as.character(read.table("data/mumin_analysis_version.txt"))


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



## Select response variables ----
response <- "iav"
# "efp" = EFPs calculated on full site timeseries
# "iav" = stability of EFPs
# "emf" = multifunctionality of EFPs


## Select subset of predictors ----
subx <- "all"
# "no"      = exclude all biodiversity variables
# "sub"     = include biodiversity but divide into ground and satellite
# "all"     = include all biodiversity predictors
# "main"    = a-posteriori analysis on significant predictors with cross validation
# "no_main" = a-posteriori analysis on significant predictors without biodiversity


## Select RaoQ input ----
raoq_in <- "nirv"
# "bands" = raoQ from all bands
# "ndvi" = raoQ from NDVI
# "nirv" = raoQ from NIRv

vers_in <- glue::glue("{response}_{subx}_{raoq_in}_{vers}")
vers_out <- vers_in



### Data ---------------------------------------------------------------------
if (response == "efp") { # analysis on full-timeseries EFPs
  dat <- read_csv(glue::glue("data/data4analysis_efps_{dat_in}.csv"), show_col_types = F) %>% glimpse()
} else if (response == "iav") { # analysis on stability of EFPs
  dat <- read_csv(glue::glue("data/data4analysis_stability_{dat_in}.csv"), show_col_types = F) %>% glimpse()
} else if (response == "emf") { # analysis on multifunctionality of EFPs
  dat <- read_csv(glue::glue("data/data4analysis_multifunctionality_{dat_in}.csv"), show_col_types = F) %>% glimpse()
}


### Vector names -------------------------------------------------------------
## Response variable(s)
if (response == "efp") { # analysis on full-timeseries EFPs
  load(file = glue::glue("data/efps_names_{efp_in}.RData"))
} else if (response == "iav") { # analysis on stability of EFPs
  load(file = glue::glue("data/efps_iav_names_{iav_in}.RData"))
} else if (response == "emf") { # analysis on multifunctionality of EFPs
  load(file = glue::glue("data/emf_names_{emf_in}.RData"))
}

## Climate
if (response == "efp") { # analysis on full-timeseries EFPs
  load(file = glue::glue("data/clim_names_{efp_in}.RData"))
} else if (response == "iav") { # analysis on stability of EFPs
  load(file = glue::glue("data/clim_names_{efp_in}.RData")) # mean climate
  load(file = glue::glue("data/clim_iav_names_{iav_in}.RData")) # climate variability
} else if (response == "emf") { # analysis on multifunctionality of EFPs
  load(file = glue::glue("data/clim_names_{efp_in}.RData"))
}

## Structure
load(file = glue::glue("data/struct_names_{efp_in}.RData"))



### Plot -----------------------------------------------------------------------
## Data ----
dat_plot <- dat %>% 
  pivot_longer(cols = all_of(efps_iav_names), names_to = "EFP_names", values_to = "EFP_values") %>% 
  relocate(EFP_names, EFP_values, Rao_Q_NIRv, .after = LONGITUDE)


## Plot ----
dat_plot %>% 
  ggplot(aes(x = Rao_Q_NIRv, y = EFP_values)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(. ~ EFP_names) +
  theme_bw() +
  theme_combine +
  theme(
    axis.title = element_blank(), # remove axes titles
    legend.background = element_rect(fill = "transparent"),
    plot.caption = element_text(color = text_color_background), # color of caption label
    plot.margin = unit(c(0, 10, 0, 0), "mm")
  ) +
  NULL