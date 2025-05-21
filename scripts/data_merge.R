#### IMPORT AND PREPARE DATA FOR ANALYSIS
# EFPs, climate, soil, vegetation structure, Rao Q...

### Author: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)
### Version History ------------------------------------------------------------
# v0300, 17.05.2024:  EFP + climate calculation v03. Multifunctionality v00. Eco stability v00 (CV).
# v0301, 17.05.2024:  EFP + climate calculation v03. Multifunctionality v00. Eco stability v01 (CV - trends).
# v0302, 17.05.2024:  EFP + climate calculation v03. Multifunctionality v00. Eco stability v02 (STD).
# v0303, 17.05.2024:  EFP + climate calculation v03. Multifunctionality v00. Eco stability v03 (STD - trends).
# v0304, 17.05.2024:  EFP + climate calculation v03. Multifunctionality v00. Eco stability v04 (CV + min_years = 10).
# v0305, 17.05.2024:  EFP + climate calculation v03. Multifunctionality v00. Eco stability v05 (CV + min_years = 7 - trends).
# v0306, 17.05.2024:  EFP + climate calculation v03. Multifunctionality v00. Eco stability v06 (STD + min_years = 7 - trends).
# v0307, 29.05.2024:  EFP + climate calculation v03. Multifunctionality v00. Eco stability v07 (inverted CV).
# v0308, 03.06.2024:  EFP + climate calculation v03. Multifunctionality v00. Eco stability v08 (inverted CV + year >= 2016 filter).


### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))

# Computation grouping strategy
group_year <- F #readline(prompt = "Calculate EFPs for site-years (T) or whole sites (F)? T/F:") # ask if sites or site-years
if (group_year %in% c("y", "yes", "Y", "YES", "T", "TRUE")) {
  grouping_var <- rlang::sym("YEAR")
  groupin <- "_ByYears"
} else if (group_year %in% c("n", "no", "N", "NO", "F", "FALSE")) {
  grouping_var <- NULL
  groupin <- ""
} else {
  print("Invalid input. Specify 'yes' or 'no'")
}
rm(group_year) # clean environment

# Data settings
efp_in <- as.character(read.table("data/efp_version.txt"))
emf_in <- as.character(read.table("data/emf_version.txt"))
# iav_in <- as.character(read.table("data/iav_version.txt"))
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
if (savedata) {
  vers_out <- paste0(
    efp_in, stringr::str_extract(emf_in, "(?<=[:digit:])[:digit:]{1}")
    #stringr::str_extract(iav_in, "(?<=[:digit:])[:digit:]{1}")
    )
  cat(paste0(vers_out, "\n"), file = "data/data_version.txt") # save version number for reference in other scripts ("\n" for new line to avoid warning when reading back into R with read.table())
  eval_file <- glue::glue("results/analysis_evaluation/evaluation_data_merging{groupin}_{vers_out}.txt")
  cat(paste0("##### IMPORT AND PREPARE DATA FOR ANALYSIS #####", "\n"), file = eval_file, append = F) # initialize evaluation text
}



### Utilities ------------------------------------------------------------------
## Packages
library(dplyr)  # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(readr)  # read csv
library(stringr)
library(tidyr)  # tidy data



### Full-timeseries EFPs -------------------------------------------------------
dat_efps <- read_csv(glue::glue("data/inter/data_efps_clim{groupin}_{efp_in}.csv"), show_col_types = F) %>% # EFPs + climate
  left_join(read_csv("data/input/igbp.csv", show_col_types = F), by = "SITE_ID") %>% # IGBP
  left_join(read_csv("data/input/coords.csv", show_col_types = F), by = "SITE_ID") %>% # coordinates
  relocate(c(IGBP, LATITUDE, LONGITUDE), .after = SITE_ID) %>% 
  left_join(read_csv("data/inter/veg_structure.csv", show_col_types = F), by = c("SITE_ID", "IGBP")) %>% # LAImax and Hc
  left_join(read_csv("data/inter/raoq/raoQ_S2_L2A_filtered_averaged.csv", show_col_types = F) %>% # Rao Q
              select(SITE_ID, contains("Rao_Q") & !contains("std")), # keep relevant metrics (exclude std, as well as NDVImax, NIRvmax...)
            by = "SITE_ID"
            )


## Evaluation ----
if (groupin == "_ByYears") {years <- "site-years"} else if (groupin == "") {years <- "sites"}

txt <- glue::glue("{nrow(drop_na(dat_efps))} {years} with full variables available.")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}

txt <- glue::glue("{nrow(dat_efps)} {years} (incl. NA entries) available.")
print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}



## Save EFP ----
if (savedata) {
  write_csv(dat_efps, glue::glue("data/inter/data4analysis_efps{groupin}_{vers_out}.csv")) # all data for full-timeseries EFPs
}



### Multifunctionality ---------------------------------------------------------
load(file = glue::glue("data/inter/emf_names_{emf_in}.RData")) # emf_names
load(file = glue::glue("data/inter/efps_names_{efp_in}.RData")) # efps_names (needed to exclude variables)

dat_emf <- read_csv(glue::glue("data/inter/data_emf_{emf_in}.csv"), show_col_types = F) %>% # EMF estimates
  left_join(read_csv("data/input/igbp.csv", show_col_types = F), by = "SITE_ID") %>% # IGBP
  left_join(read_csv("data/input/coords.csv", show_col_types = F), by = "SITE_ID") %>% # coordinates
  left_join(read_csv("data/inter/veg_structure.csv", show_col_types = F), by = c("SITE_ID", "IGBP")) %>% # LAImax and Hc
  left_join(read_csv("data/inter/raoq/raoQ_S2_L2A_filtered_averaged.csv", show_col_types = F) %>% # Rao Q
              select(SITE_ID, contains("Rao_Q") & !contains("std")), # keep relevant metrics (exclude std, as well as NDVImax, NIRvmax...)
            by = "SITE_ID"
  ) %>%
  left_join(read_csv(glue::glue("data/inter/data_efps_clim{groupin}_{efp_in}.csv"), show_col_types = F) %>% # include constant climatic variables
              select(-all_of(efps_names), -contains("pval"), - contains("NEP99")),
            by = "SITE_ID"
  ) %>% 
  relocate(c(IGBP, LATITUDE, LONGITUDE), .after = SITE_ID)


## Save EMF ----
if (savedata) {
  write_csv(dat_emf, glue::glue("data/inter/data4analysis_multifunctionality_{vers_out}.csv")) # data for multifunctionality of EFPs
}



### End ------------------------------------------------------------------------