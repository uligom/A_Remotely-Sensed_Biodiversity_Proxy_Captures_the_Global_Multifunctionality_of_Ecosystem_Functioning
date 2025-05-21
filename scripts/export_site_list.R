#### EXTRACT SITE LIST

### Authors: Ulisse Gomarasca
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))


# Data settings
efp_in <- as.character(read.table("data/efp_version.txt"))
emf_in <- as.character(read.table("data/emf_version.txt"))
vers_in <- as.character(read.table("data/global_version.txt"))

savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
if (savedata) {
  vers_out <- vers_in # output version
}



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)      # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(glue)       # glue strings
library(readr)      # read csv files

## Themes
source("scripts/themes/MyCols.R")



### Data -----------------------------------------------------------------------
dat_efps <- read_csv(glue("data/inter/data_efps_clim_{efp_in}.csv"), show_col_types = F)
dat_emf <- read_csv(glue("data/inter/data_emf_{emf_in}.csv"), show_col_types = F)
# dat_iav <- read_csv(glue("data/data4analysis_stability_{vers_in}.csv"), show_col_types = F)

# dat_years <- read_csv(glue("data/data4analysis_ByYears_{vers_in}.csv"), show_col_types = F)



### Isolate SITE_ID ------------------------------------------------------------
sites <- bind_rows(
  dat_efps %>% select(SITE_ID),
  dat_emf %>% select(SITE_ID),
  # dat_iav %>% select(SITE_ID)
  # dat_years %>% select(SITE_ID)
  ) %>%
  unique()
sites



### Save -----------------------------------------------------------------------
if (savedata) {
  write_csv(sites, "data/inter/site_list_all.csv")
}