#### PLOT RaoQ vs taxonomic diversity

### Authors: Ulisse Gomarasca
### Script start ---------------------------------------------------------------
# Clear environment
rm(list = ls(all = T))



### Options --------------------------------------------------------------------
# Data settings
savedata <- F #as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
dat_in <- "v0300" #as.character(read.table("data/data_version.txt"))
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
source("scripts/functions/plot_scatterplot_models.R")
source("scripts/functions/plot_best.R")
source("scripts/themes/MyThemes.R")
source("scripts/themes/MyCols.R")
source("scripts/themes/MyPlotSpecs.R")



### Data -----------------------------------------------------------------------
dat_spp <- read_csv("//minerva/BGI/people/ugomar/R/scale_emergent_properties/data/species_ECsites/species_cover_v12_release.csv", show_col_types = F) %>% 
  select(SITE_ID, n_spp, shannon, evenness) %>% 
  unique()

dat <- read_csv(glue::glue("data/data4analysis_efps_{dat_in}.csv"), show_col_types = F) %>% 
  left_join(dat_spp, by = "SITE_ID") %>% 
  glimpse()



### Plot -----------------------------------------------------------------------
plot_best(data = dat, x = n_spp, y = Rao_Q_NIRv)
plot_best(data = dat, x = shannon, y = Rao_Q_NIRv)
plot_best(data = dat, x = evenness, y = Rao_Q_NIRv)