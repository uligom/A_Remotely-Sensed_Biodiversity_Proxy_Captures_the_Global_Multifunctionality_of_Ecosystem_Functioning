#### EXTRACT IGBP CLASS AND SITE COORDINATES

### Authors: Ulisse Gomarasca
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

# # Detach packages for a clean start
# if (!is.null(sessionInfo()$otherPkgs)) {invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach, character.only = T, unload = T))}

# Start
library(tictoc)
tic("") # Script run time.

# Data settings
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
if (savedata) {
  eval_file <- glue::glue("results/analysis_evaluation/evaluation_extract_site_igbp_coords.txt")
  cat(paste0("##### IMPORT AND EXTRACT SITE INFORMATION #####", "\n"), file = eval_file, append = F) # initialize evaluation text
}



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)      # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(furrr)      # map in parallel
library(future)     # plan parallelization
library(ggplot2)    # tidy plots
library(janitor)    # clean data
library(lubridate)  # dates
library(ncdf4)      # netcdf files
library(purrr)      # map functions
library(quantreg)   # quantile regression
library(readr)      # read csv files
library(rlang)      # quoting inside functions
library(stringr)    # tidy string manipulation
library(tictoc)     # measure time
library(tidyr)      # clean and reshape tidy data

## Functions
source("scripts/functions/import_COORDS.R")
source("scripts/functions/import_IGBP.R")



### Data files -----------------------------------------------------------------
data_path <- "//minerva/BGI/work_4/scratch/jnelson/4Sinikka/data20240123/"
data_path2 <- "//minerva/BGI/work_1/scratch/fluxcom/sitecube_proc/model_files/"

## Sites
sites <- str_extract(list.files(data_path), pattern = "[:upper:]{2}-[:alnum:]{3}") %>% unique()
sites2 <- str_extract(list.files(data_path2), pattern = "[:upper:]{2}-[:alnum:]{3}") %>% unique()


## Combine site list
# TO DO: check what is missing, and possibly integrate from other sources
sites <- intersect(sites, sites2); rm(sites2)



### Sites of Interest ----------------------------------------------------------
# Sites of interest (mostly for testing without the full site list)
rangesites <- 1:length(sites)
site_list <- split(sites[rangesites], seq(length(sites[rangesites]))); # site_list <- as.list(sites[rangesites]) # simpler, check later if it works



#### Parallelization -----------------------------------------------------------
# ## Test function ----
# igbp_out <- import_IGBP(site_list = site_list[1], path = data_path2)
# # works as of 05.02.2024 10:27 by full sites or single site-years
# coords_out <- import_COORDS(site_list = site_list[1], path = data_path)
# # works as of 04.03.2024 15:21 by single site-years



## Define computing strategy ----
if (getwd() == "C:/Users/ugomar/Desktop/multifunctionality") {
  plan(multisession, workers = 2) # works as of 26.01.2024 17:05
} else if (getwd() == "/Net/Groups/BGI/people/ugomar/R/B_EF") {
  ncpus <- as.integer(readline(prompt = "How many CPUs should be used for parallel computing? (Careful not to crash your computer!)"))
  plan(cluster, workers = ncpus)
}


### Run functions (parallel computing) -----------------------------------------
## Extract IGBP ----
txt <- glue::glue("### Import of IGBP class ###")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}

tic("")
igbp_out <- future_map_dfr( # output as row-bound dataframe
  .x = site_list, .f = import_IGBP, path = data_path2,
  .options = furrr_options(
    seed = 1234, scheduling = Inf, chunk_size = NULL), # scheduling set to Inf creates n_x chunks (1 chunk per element of .x).
  .progress = T
)
toc(log = T); log_igbp <- tic.log(format = T)[[1]]; tic.clearlog()



## Extract Coordinates ----
txt <- glue::glue("### Import of coordinates ###")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}

tic("")
coords_out <- future_map_dfr( # output as row-bound dataframe
  .x = site_list, .f = import_COORDS, path = data_path,
  .options = furrr_options(
    seed = 1234, scheduling = Inf, chunk_size = NULL), # scheduling set to Inf creates n_x chunks (1 chunk per element of .x).
  .progress = T
)
toc(log = T); log_coords <- tic.log(format = T)[[1]]; tic.clearlog()



## Revert to default computing strategy to avoid issues ----
plan(sequential)



## Remove empty output ----
igbp_out <- igbp_out[rowSums(is.na(igbp_out[, 0:ncol(igbp_out)])) < ncol(igbp_out) - 1, ] # remove empty rows

coords_out <- coords_out[rowSums(is.na(coords_out[, 0:ncol(coords_out)])) < ncol(coords_out) - 1, ] # remove empty rows



### Save -----------------------------------------------------------------------
if (savedata) {
  write_csv(igbp_out, glue::glue("data/input/igbp.csv")) # igbp
  write_csv(coords_out, glue::glue("data/input/coords.csv")) # coords
}



### End ------------------------------------------------------------------------
toc(log = T)
txt <- glue::glue("Script total run time: {tic.log(format = T)[[1]]}."); tic.clearlog()
if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}