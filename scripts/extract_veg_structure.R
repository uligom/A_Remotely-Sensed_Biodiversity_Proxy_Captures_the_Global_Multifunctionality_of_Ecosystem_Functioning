#### IMPORT ANCILLARY DATA
# LAI, canopy height...

### Author: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)
### Script settings ------------------------------------------------------------
# Clear environment
rm(list = ls(all = TRUE))


# Data settings
vers_in <- as.character(read.table("data/efp_version.txt"))
savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
if (savedata) {vers_out <- vers_in}



### Utilities ------------------------------------------------------------------
## Packages
library(binaryLogic)# bit binary conversion
library(dplyr)      # tidy data manipulation
options(dplyr.summarise.inform = F) # suppress summary info
library(ggplot2)    # tidy plots
library(glue)       # glue strings
library(lubridate)  # dates
library(ncdf4)      # netcdf files
library(purrr)      # map functions
library(readr)      # read csv files
library(readxl)     # excel files
library(stringr)    # tidy string manipulation
library(tidyr)      # clean and reshape tidy data

## Functions
source("scripts/functions/plot_scatter_lm.R")



### Sites ----------------------------------------------------------------------
my_sites <- read_csv("data/inter/site_list_all.csv", show_col_types = F) %>% pull(SITE_ID) # available sites with EFPs



### Import & processing --------------------------------------------------------
## Ameriflux (open source) ----
file_list <- list.files("data/input/Flux_networks/Ameriflux/CC4.0/", pattern = "AMF_")
site_list <- str_extract(file_list, "[:upper:]{2}-[:alnum:]{3}"); site_list_amf <- site_list
date_list <- str_extract(file_list, "[:digit:]{8}")[site_list %in% my_sites] # needed to build file path
site_list <- site_list[site_list %in% my_sites]

dat_amf <- tibble()
for (i in 1:length(site_list)) {
  dat_amf <- bind_rows(dat_amf, read_xlsx(glue("data/input/Flux_networks/Ameriflux/CC4.0/AMF_{site_list[i]}_BIF_{date_list[i]}.xlsx")))
}
# Tidy
dat_amf <- dat_amf %>% 
  pivot_wider(id_cols = c(SITE_ID, GROUP_ID), names_from = VARIABLE, values_from = DATAVALUE) %>% 
  arrange(SITE_ID, GROUP_ID)

## Select variables of interest and aggregate to site values
dat_amf <- full_join(
  dat_amf %>% ## Canopy height
    select(SITE_ID, GROUP_ID, contains("HEIGHTC")) %>% 
    drop_na(HEIGHTC) %>% 
    rename(DATE = HEIGHTC_DATE) %>% 
    mutate(
      DATE = case_when(
        str_detect(DATE, "^[:digit:]{4}$") ~ paste0(DATE, "0101"), # only year present
        str_detect(DATE, "^[:digit:]{6}$") ~ paste0(DATE, "01"), # only year-month present
        str_detect(DATE, "^[:digit:]{8}$") ~ DATE, # full date present
        .default = "19000101"
      ),
      DATE = as_date(DATE, format = "%Y%m%d"),
      .after = SITE_ID
    ) %>%
    mutate(YEAR = year(DATE)) %>% 
    group_by(SITE_ID, YEAR) %>% # site-years
    summarise(Hc = mean(as.double(HEIGHTC), na.rm = T), .groups = "drop") %>% 
    group_by(SITE_ID) %>% # full sites
    summarise(Hc = mean(Hc, na.rm = T), .groups = "drop"),
  dat_amf %>% ## LAImax
    select(SITE_ID, contains("LAI")) %>% 
    drop_na(LAI_TOT) %>% # LAI_U (understory) would still be present, but no dates are given for that variable
    rename(DATE = LAI_DATE) %>% 
    mutate(
      DATE = case_when(
        str_detect(DATE, "^[:digit:]{4}$") ~ paste0(DATE, "0101"), # only year present
        str_detect(DATE, "^[:digit:]{6}$") ~ paste0(DATE, "01"), # only year-month present
        str_detect(DATE, "^[:digit:]{8}$") ~ DATE, # full date present
        .default = "19000101"
      ),
      DATE = as_date(DATE, format = "%Y%m%d"),
      .after = SITE_ID
    ) %>% 
    group_by(SITE_ID) %>% 
    summarise(LAImax = quantile(as.double(LAI_TOT), 0.95), .groups = "drop"),
  by = c("SITE_ID")
) %>% 
  mutate(SOURCE = "Ameriflux BIF")


# ## Ameriflux (restricted access) ----
# # NB: Only 4 additional sites compared to CC 4.0 subset, and they are already included in FLUXNET BADM or Gomarasca et al., 2023
# file_list <- list.files("data/input/Flux_networks/Ameriflux/restricted_access/", pattern = "AMF_")
# site_list <- str_extract(file_list, "[:upper:]{2}-[:alnum:]{3}")
# date_list <- str_extract(file_list, "[:digit:]{8}")[site_list %in% my_sites & !(site_list %in% site_list_amf)] # needed to build file path
# site_list <- site_list[!site_list %in% site_list_amf & site_list %in% my_sites]
# 
# dat_amf_legacy <- tibble()
# for (i in 1:length(site_list)) {
#   dat_amf_legacy <- bind_rows(dat_amf_legacy, read_xlsx(glue("data/input/Flux_networks/Ameriflux/restricted_access/AMF_{site_list[i]}_BIF_{date_list[i]}.xlsx")))
# }
# # Tidy
# dat_amf_legacy <- dat_amf_legacy %>% 
#   pivot_wider(id_cols = c(SITE_ID, GROUP_ID), names_from = VARIABLE, values_from = DATAVALUE) %>% 
#   arrange(SITE_ID, GROUP_ID)
# 
# ## Select variables of interest and aggregate to site values
# dat_amf_legacy <- full_join(
#   dat_amf_legacy %>% ## Canopy height
#     select(SITE_ID, GROUP_ID, contains("HEIGHTC")) %>% 
#     drop_na(HEIGHTC) %>% 
#     rename(DATE = HEIGHTC_DATE) %>% 
#     mutate(
#       DATE = case_when(
#         str_detect(DATE, "^[:digit:]{4}$") ~ paste0(DATE, "0101"), # only year present
#         str_detect(DATE, "^[:digit:]{6}$") ~ paste0(DATE, "01"), # only year-month present
#         str_detect(DATE, "^[:digit:]{8}$") ~ DATE, # full date present
#         .default = "19000101"
#       ),
#       DATE = as_date(DATE, format = "%Y%m%d"),
#       .after = SITE_ID
#     ) %>%
#     mutate(YEAR = year(DATE)) %>% 
#     group_by(SITE_ID, YEAR) %>% # site-years
#     summarise(Hc = mean(as.double(HEIGHTC), na.rm = T), .groups = "drop") %>% 
#     group_by(SITE_ID) %>% # full sites
#     summarise(Hc = mean(Hc, na.rm = T), .groups = "drop"),
#   dat_amf_legacy %>% ## LAImax
#     select(SITE_ID, contains("LAI")) %>% 
#     drop_na(LAI_TOT) %>%
#     mutate(LAImax = as.double(LAI_TOT)) %>% 
#     select(SITE_ID, LAImax),
#   by = c("SITE_ID")
# )
# 
# 

## NEON ----
dat_neon <- read_csv("data/input/Flux_networks/NEON/NEONstructure_extracted_v03.csv", show_col_types = F) %>% # extracted structure (mean Hc)
  rename(Hc = height) %>% 
  full_join(
    read_csv("data/input/Flux_networks/NEON/LAI/LAImax_GBOV_NEON_extracted.csv", show_col_types = F), # extracted LAImax
    by = "SITE_ID"
  ) %>% 
  rename(NEON_ID = SITE_ID) %>% 
  ## translate site IDs
  left_join(
    read_csv("data/input/Flux_networks/NEON/NEON-AMERIFLUX_siteID_translations.csv", show_col_types = F), # site ID translations NEON-FLUXNET
    by = "NEON_ID"
  ) %>% 
  select(SITE_ID, Hc, LAImax) %>% 
  mutate(SOURCE = "https://land.copernicus.eu/global/gbov")


## ICOS ----
file_list <- list.files("//minerva/BGI/people/ugomar/codes/B_EF/data/input/Flux_networks/ICOS", pattern = "ICOSETC")
site_list <- stringr::str_extract(file_list, "[:upper:]{2}-[:alnum:]{3}")

dat_icos <- tibble()
for (i in 1:length(file_list)) {
  dat_icos <- bind_rows(
    dat_icos,
    read_csv(unzip(glue("//minerva/BGI/people/ugomar/codes/B_EF/data/input/Flux_networks/ICOS/{file_list[i]}"),
                   glue("ICOSETC_{site_list[i]}_ANCILLARY_L2.csv"),
                   exdir = "/data/input/Flux_networks/ICOS"),
             show_col_types = F
             )
  )
}
# Tidy
dat_icos <- dat_icos %>% 
  dplyr::filter(VARIABLE != "SITE_ID") %>% 
  pivot_wider(id_cols = c(SITE_ID, GROUP_ID), names_from = VARIABLE, values_from = DATAVALUE) %>% 
  arrange(SITE_ID, GROUP_ID)

## Select variables of interest and aggregate to site values
dat_icos <- full_join(
  dat_icos %>% ## Canopy height
    select(SITE_ID, GROUP_ID, contains("HEIGHTC")) %>% 
    drop_na(HEIGHTC) %>% 
    dplyr::filter(HEIGHTC_STATISTIC %in% c("Mean", "50th Percentile", "Single observation")) %>% 
    rename(DATE = HEIGHTC_DATE) %>% 
    mutate(
      DATE = case_when(
        str_detect(DATE, "^[:digit:]{4}$") ~ paste0(DATE, "0101"), # only year present
        str_detect(DATE, "^[:digit:]{6}$") ~ paste0(DATE, "01"), # only year-month present
        str_detect(DATE, "^[:digit:]{8}$") ~ DATE, # full date present
        is.na(DATE) & !is.na(HEIGHTC_DATE_START) ~ HEIGHTC_DATE_START, # no data in DATE
        .default = "19000101"
      ),
      DATE = as_date(DATE, format = "%Y%m%d")
    ) %>%
    relocate(DATE, .after = SITE_ID) %>% 
    mutate(YEAR = year(DATE), .after = SITE_ID) %>%
    group_by(SITE_ID, YEAR) %>% # site-years
    summarise(Hc = mean(as.double(HEIGHTC), na.rm = T), .groups = "drop") %>% 
    group_by(SITE_ID) %>% # full sites
    summarise(Hc = mean(Hc, na.rm = T), .groups = "drop"),
  dat_icos %>% ## LAImax
    select(SITE_ID, contains("LAI")) %>% 
    drop_na(LAI) %>%
    dplyr::filter(LAI_STATISTIC %in% c("Mean", "50th Percentile", "Single observation")) %>% 
    rename(DATE = LAI_DATE) %>% 
    mutate(
      DATE = case_when(
        str_detect(DATE, "^[:digit:]{4}$") ~ paste0(DATE, "0101"), # only year present
        str_detect(DATE, "^[:digit:]{6}$") ~ paste0(DATE, "01"), # only year-month present
        str_detect(DATE, "^[:digit:]{8}$") ~ DATE, # full date present
        is.na(DATE) & !is.na(LAI_DATE_START) ~ LAI_DATE_START, # no data in DATE
        .default = "19000101"
      ),
      DATE = as_date(DATE, format = "%Y%m%d"),
      .after = SITE_ID
    ) %>% 
    group_by(SITE_ID) %>% 
    summarise(LAImax = quantile(as.double(LAI), 0.95), .groups = "drop"),
  by = c("SITE_ID")
) %>% 
  mutate(SOURCE = "ICOS BIF ANCILLARY")


## OzFlux ----
dat_ozf <- bind_rows(
  tibble(SITE_ID = "AU-ASM", Hc = 6.5, SOURCE = "https://www.ozflux.org.au/monitoringsites/alicesprings/alicesprings_description.html"),
  tibble(SITE_ID = "AU-Cpr", Hc = 3, SOURCE = "https://data.ozflux.org.au/portal/pub/viewColDetails.jspx?collection.id=1882712&collection.owner.id=703&viewType=anonymous"),
  tibble(SITE_ID = "AU-Cum", Hc = 23, SOURCE = "https://ozflux.org.au/monitoringsites/cumberlandplain/cumberlandplain_description.html"),
  tibble(SITE_ID = "AU-DaP", Hc = 0.7, SOURCE = "https://ozflux.org.au/monitoringsites/dalypasture/dalypasture_description.html"),
  tibble(SITE_ID = "AU-DaS", Hc = 16.4, SOURCE = "https://ozflux.org.au/monitoringsites/dalyuncleared/dalyuncleared_description.html"),
  tibble(SITE_ID = "AU-Dry", Hc = 12.3, SOURCE = "https://ozflux.org.au/monitoringsites/dryriver/dryriver_description.html"),
  tibble(SITE_ID = "AU-Emr", Hc = 0, SOURCE = "https://ozflux.org.au/monitoringsites/arcturus/arcturus_description.html"),
  tibble(SITE_ID = "AU-Gin", Hc = 7, LAImax = 0.8, SOURCE = "https://ozflux.org.au/monitoringsites/gingin/gingin_description.html"),
  tibble(SITE_ID = "AU-GWW", Hc = 18, SOURCE = "https://data.ozflux.org.au/portal/pub/viewColDetails.jspx?collection.id=750&collection.owner.id=2024852&viewType=anonymous"),
  tibble(SITE_ID = "AU-How", Hc = 15, SOURCE = "https://ozflux.org.au/monitoringsites/howardsprings/howardsprings_description.html"),
  tibble(SITE_ID = "AU-Lox", Hc = 5.5, SOURCE = "https://geonetwork.tern.org.au/geonetwork/srv/api/records/c5df7669-ef1e-49a3-a00d-9825860ea010"),
  tibble(SITE_ID = "AU-RDF", Hc = 16, SOURCE = "https://data.ozflux.org.au/portal/pub/viewColDetails.jspx?collection.id=1882721&collection.owner.id=304&viewType=anonymous"),
  tibble(SITE_ID = "AU-Rig", Hc = 0.4, SOURCE = "https://data.ozflux.org.au/portal/pub/viewColDetails.jspx?collection.id=1882722&collection.owner.id=304&viewType=anonymous"),
  tibble(SITE_ID = "AU-Rob", Hc = 28, SOURCE = "https://data.ozflux.org.au/portal/pub/viewColDetails.jspx?collection.id=1882719&collection.owner.id=100&viewType=anonymous"),
  tibble(SITE_ID = "AU-Stp", Hc = 0.5, SOURCE = "https://data.ozflux.org.au/portal/pub/viewColDetails.jspx?collection.id=1882705&collection.owner.id=304&viewType=anonymous"),
  tibble(SITE_ID = "AU-TTE", Hc = 4.85, SOURCE = "https://ozflux.org.au/monitoringsites/titreeeast/titreeeast_description.html"),
  tibble(SITE_ID = "AU-Whr", Hc = 28, SOURCE = "https://data.ozflux.org.au/portal/pub/viewColDetails.jspx?collection.id=1882707&collection.owner.id=304&viewType=anonymous"),
  tibble(SITE_ID = "AU-Wom", Hc = 25, SOURCE = "https://ozflux.org.au/monitoringsites/wombat/wombat_description.html"),
  tibble(SITE_ID = "AU-Ync", Hc = 0.5, SOURCE = "https://data.ozflux.org.au/portal/pub/viewColDetails.jspx?collection.id=1882711&collection.owner.id=304&viewType=anonymous")
) #%>% 
  # select(-SOURCE)


## Gomarasca et al., 2023 ----
dat_gom <- read_csv("//minerva/BGI/people/ugomar/codes/scale_emergent_properties/data/intermediate/multivar_mean_v13.csv", show_col_types = F) %>% 
  select(SITE_ID, Hc, LAImax) %>% 
  mutate(SOURCE = "Gomarasca, U., Migliavacca, M., Kattge, J., Nelson, J. A., Niinemets, Ü., Wirth, C., Cescatti, A., Bahn, M., Nair, R., Acosta, A. T. R., Arain, M. A., Beloiu, M., Black, T. A., Bruun, H. H., Bucher, S. F., Buchmann, N., Byun, C., Carrara, A., Conte, A., … Reichstein, M. (2023). Leaf-level coordination principles propagate to the ecosystem scale. Nature Communications, 14(1), Article 1. https://doi.org/10.1038/s41467-023-39572-5")


## FLUXNET ----
dat_flxn <- read_csv("data/input/Flux_networks/FLUXNETbgi_BADM.csv", show_col_types = F) %>% 
  rename(SITE_ID = siteID, LAImax = LAI_TOT, Hc = HEIGHTC) %>% 
  select(SITE_ID, LAImax, Hc) %>% 
  mutate(SOURCE = "FLUXNET BADM")


## Literature ----
dat_lit <- bind_rows(
  tibble(SITE_ID = "AR-Slu", Hc = 6, LAImax = NA_real_, SOURCE = "Nosetto, M. D., Luna Toledo, E., Magliano, P. N., Figuerola, P., Blanco, L. J., & Jobbágy, E. G. (2020). Contrasting CO2 and water vapour fluxes in dry forest and pasture sites of central Argentina. Ecohydrology, 13(8), e2244. https://doi.org/10.1002/eco.2244"),
  tibble(SITE_ID = "BE-Dor", Hc = 0.1, LAImax = NA_real_, SOURCE = "Mamadou, O., Gourlez De La Motte, L., De Ligne, A., Heinesch, B., & Aubinet, M. (2016). Sensitivity of the annual net ecosystem exchange to the cospectral model used for high frequency loss corrections at a grazed grassland site. Agricultural and Forest Meteorology, 228–229, 360–369. https://doi.org/10.1016/j.agrformet.2016.06.008"),
  tibble(SITE_ID = "BR-Npw", Hc = 6, LAImax = 7.4, SOURCE = "Dalmagro, H. J., Zanella de Arruda, P. H., Vourlitis, G. L., Lathuillière, M. J., de S. Nogueira, J., Couto, E. G., & Johnson, M. S. (2019). Radiative forcing of methane fluxes offsets net carbon dioxide uptake for a tropical flooded forest. Global Change Biology, 25(6), 1967–1981. https://doi.org/10.1111/gcb.14615"),
  tibble(SITE_ID = "CA-DB2", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "CA-DBB", Hc = 0.3, LAImax = NA_real_, SOURCE = "Tortini, R., Coops, N. C., Nesic, Z., Christen, A., Lee, S. C., & Hilker, T. (2017). Remote sensing of seasonal light use efficiency in temperate bog ecosystems. Scientific Reports, 7(1), Article 1. https://doi.org/10.1038/s41598-017-08102-x"),
  tibble(SITE_ID = "CA-LP1", Hc = 15, LAImax = NA_real_, SOURCE = "https://ameriflux.lbl.gov/sites/siteinfo/CA-LP1"),
  tibble(SITE_ID = "CH-Aws", Hc = 0.25, LAImax = 4, SOURCE = "https://www.swissfluxnet.ethz.ch/index.php/sites/site-info-ch-aws/"),
  tibble(SITE_ID = "CN-Dan", Hc = 0.1, LAImax = NA_real_, SOURCE = "Tang, X., Zhou, Y., Li, H., Yao, L., Ding, Z., Ma, M., & Yu, P. (2020). Remotely monitoring ecosystem respiration from various grasslands along a large-scale east–west transect across northern China. Carbon Balance and Management, 15, 6. https://doi.org/10.1186/s13021-020-00141-8"),
  tibble(SITE_ID = "CN-Du3", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "CZ-Lnz", Hc = 27, LAImax = NA_real_, SOURCE = "Prikaziuk, E., Migliavacca, M., Su, Z. (Bob), & van der Tol, C. (2023). Simulation of ecosystem fluxes with the SCOPE model: Sensitivity to parametrization and evaluation with flux tower observations. Remote Sensing of Environment, 284, 113324. https://doi.org/10.1016/j.rse.2022.113324"),
  tibble(SITE_ID = "CZ-RAJ", Hc = 17, LAImax = NA_real_, SOURCE = "Prikaziuk, E., Migliavacca, M., Su, Z. (Bob), & van der Tol, C. (2023). Simulation of ecosystem fluxes with the SCOPE model: Sensitivity to parametrization and evaluation with flux tower observations. Remote Sensing of Environment, 284, 113324. https://doi.org/10.1016/j.rse.2022.113324"),
  tibble(SITE_ID = "CZ-Stn", Hc = 13, LAImax = NA_real_, SOURCE = "Prikaziuk, E., Migliavacca, M., Su, Z. (Bob), & van der Tol, C. (2023). Simulation of ecosystem fluxes with the SCOPE model: Sensitivity to parametrization and evaluation with flux tower observations. Remote Sensing of Environment, 284, 113324. https://doi.org/10.1016/j.rse.2022.113324"),
  tibble(SITE_ID = "DE-Hte", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "DE-Hzd", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "ES-Abr", Hc = 8, LAImax = NA_real_, SOURCE = "Prikaziuk, E., Migliavacca, M., Su, Z. (Bob), & van der Tol, C. (2023). Simulation of ecosystem fluxes with the SCOPE model: Sensitivity to parametrization and evaluation with flux tower observations. Remote Sensing of Environment, 284, 113324. https://doi.org/10.1016/j.rse.2022.113324"),
  tibble(SITE_ID = "ES-Agu", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "ES-LM1", Hc = 8, LAImax = NA_real_, SOURCE = "Prikaziuk, E., Migliavacca, M., Su, Z. (Bob), & van der Tol, C. (2023). Simulation of ecosystem fluxes with the SCOPE model: Sensitivity to parametrization and evaluation with flux tower observations. Remote Sensing of Environment, 284, 113324. https://doi.org/10.1016/j.rse.2022.113324"),
  tibble(SITE_ID = "ES-LM2", Hc = 8, LAImax = NA_real_, SOURCE = "Prikaziuk, E., Migliavacca, M., Su, Z. (Bob), & van der Tol, C. (2023). Simulation of ecosystem fluxes with the SCOPE model: Sensitivity to parametrization and evaluation with flux tower observations. Remote Sensing of Environment, 284, 113324. https://doi.org/10.1016/j.rse.2022.113324"),
  tibble(SITE_ID = "FI-Sii", Hc = 0.3, LAImax = NA_real_, SOURCE = "Prikaziuk, E., Migliavacca, M., Su, Z. (Bob), & van der Tol, C. (2023). Simulation of ecosystem fluxes with the SCOPE model: Sensitivity to parametrization and evaluation with flux tower observations. Remote Sensing of Environment, 284, 113324. https://doi.org/10.1016/j.rse.2022.113324"),
  tibble(SITE_ID = "FR-FBn", Hc = 10, LAImax = NA_real_, SOURCE = "Prikaziuk, E., Migliavacca, M., Su, Z. (Bob), & van der Tol, C. (2023). Simulation of ecosystem fluxes with the SCOPE model: Sensitivity to parametrization and evaluation with flux tower observations. Remote Sensing of Environment, 284, 113324. https://doi.org/10.1016/j.rse.2022.113324"),
  tibble(SITE_ID = "FR-Mej", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "GL-NuF", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "GL-ZaF", Hc = 0.15, LAImax = 1.3, SOURCE = "Soegaard, H., & Nordstroem, C. (1999). Carbon dioxide exchange in a high-arctic fen estimated by eddy covariance measurements and modelling. Global Change Biology, 5(5), 547–562. https://doi.org/10.1111/j.1365-2486.1999.00250.x"),
  tibble(SITE_ID = "GL-ZaH", Hc = NA_real_, LAImax = 0.25, SOURCE = "Lund, M., Falk, J. M., Friborg, T., Mbufong, H. N., Sigsgaard, C., Soegaard, H., & Tamstorf, M. P. (2012). Trends in CO2 exchange in a high Arctic tundra heath, 2000–2010. Journal of Geophysical Research: Biogeosciences, 117(G2). https://doi.org/10.1029/2011JG001901"),
  tibble(SITE_ID = "IT-Isp", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "IT-Lsn", Hc = 2, LAImax = NA_real_, SOURCE = "Prikaziuk, E., Migliavacca, M., Su, Z. (Bob), & van der Tol, C. (2023). Simulation of ecosystem fluxes with the SCOPE model: Sensitivity to parametrization and evaluation with flux tower observations. Remote Sensing of Environment, 284, 113324. https://doi.org/10.1016/j.rse.2022.113324"),
  tibble(SITE_ID = "RU-Fy2", Hc = 27.4, LAImax = NA_real_, SOURCE = "Prikaziuk, E., Migliavacca, M., Su, Z. (Bob), & van der Tol, C. (2023). Simulation of ecosystem fluxes with the SCOPE model: Sensitivity to parametrization and evaluation with flux tower observations. Remote Sensing of Environment, 284, 113324. https://doi.org/10.1016/j.rse.2022.113324"),
  tibble(SITE_ID = "SE-Ros", Hc = 14, LAImax = NA_real_, SOURCE = "Prikaziuk, E., Migliavacca, M., Su, Z. (Bob), & van der Tol, C. (2023). Simulation of ecosystem fluxes with the SCOPE model: Sensitivity to parametrization and evaluation with flux tower observations. Remote Sensing of Environment, 284, 113324. https://doi.org/10.1016/j.rse.2022.113324"),
  tibble(SITE_ID = "SJ-Adv", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-A32", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-BZB", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-BZF", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-BZo", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-BZS", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-EDN", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-Hn3", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-HWB", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-ICs", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-ICt", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-Jo2", Hc = 1, LAImax = NA_real_, SOURCE = "Schreiner-McGraw, A. P., & Vivoni, E. R. (2017). Percolation observations in an arid piedmont watershed and linkages to historical conditions in the Chihuahuan Desert. Ecosphere, 8(11), e02000. https://doi.org/10.1002/ecs2.2000"),
  tibble(SITE_ID = "US-KFS", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-KS3", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-Mpj", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-NC4", Hc = 17.5, LAImax = 3.5, SOURCE = "Miao, G., Noormets, A., Domec, J.-C., Trettin, C. C., McNulty, S. G., Sun, G., & King, J. S. (2013). The effect of water table fluctuation on soil respiration in a lower coastal plain forested wetland in the southeastern U.S. Journal of Geophysical Research: Biogeosciences, 118(4), 1748–1762. https://doi.org/10.1002/2013JG002354"),
  tibble(SITE_ID = "US-ONA", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-Ro4", Hc = 2.7, LAImax = 6.4, SOURCE = "Griffis, T. J., Lee, X., Baker, J. M., Billmark, K., Schultz, N., Erickson, M., Zhang, X., Fassbinder, J., Xiao, W., & Hu, N. (2011). Oxygen isotope composition of evapotranspiration and its relation to C4 photosynthetic discrimination. Journal of Geophysical Research: Biogeosciences, 116(G1). https://doi.org/10.1029/2010JG001514"),
  tibble(SITE_ID = "US-Seg", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-Ses", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-Sne", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-Snf", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-Wi7", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_),
  tibble(SITE_ID = "US-xAE", Hc = 1, LAImax = NA_real_, SOURCE = "https://www.neonscience.org/field-sites/oaes"),
  tibble(SITE_ID = "US-xKA", Hc = NA_real_, LAImax = NA_real_, SOURCE = NA_character_)
) #%>% 
  # select(-SOURCE)



### MODIS LAI ------------------------------------------------------------------
## Data from Google Earth Engine
# https://developers.google.com/earth-engine/datasets/catalog/MODIS_061_MCD15A3H
modis_lai <- read_csv("data/input/MODIS_LAI.csv", show_col_types = F) %>% 
  dplyr::select(-`system:index`, -.geo) %>% 
  relocate(SITE_ID, date, .before = everything()) %>% 
  arrange(SITE_ID, date) %>% 
  drop_na() %>%
  glimpse()

## Filter and calculate LAImax
modis_lai <- modis_lai %>%
  mutate(
    across(.cols = contains("_QC"), .fns = ~ binaryLogic::as.binary(., n = 8)), # convert decimal to binary numbers (8-bits)
    across(.cols = contains("_QC"), .fns = ~ map(.x = ., .f = ~ paste0(., collapse = "")) %>% unlist()) # convert binary to character
  ) %>% 
  dplyr::filter(
    str_detect(FparLai_QC, pattern = "00[:digit:]{1}000[:digit:]{1}0"), # FparLai_QC
    # bit 0 = good quality, bit 1 = both sensors, bit 2 = only fine detectors, bits 3-4 = no clouds, bits 5-7 = main method with or without saturation
    str_detect(FparExtra_QC, pattern = "[:digit:]{1}00000[:digit:]{2}"), # FparExtra_QC
    # bit 0-1 = any land cover, bit 2 = no snow/ice, bit 3 = no aerosol, bit 4 = no cirrus, bit 5 = no clouds, bit 6 = no shadows, bit 7 = any biome
  ) %>%
  group_by(SITE_ID) %>%
  summarise(LAImax_modis = quantile(as.double(central_LAI), 0.95, na.rm = T),
            # local_LAImax = quantile(as.double(local_mean_LAI), 0.95, na.rm = T), # should be filtered before averaging in GEE to be usable
            .groups = "drop"
            ) %>%
  glimpse()

hist(modis_lai$LAImax_modis)



### Merge data -----------------------------------------------------------------
dat <- tibble(SITE_ID = my_sites) %>% 
  left_join(bind_rows(dat_amf, dat_neon, dat_icos, dat_ozf), by = "SITE_ID") %>% # data from single networks
  left_join(dat_gom, by = "SITE_ID", suffix = c("", "_b")) %>% # curated data from Gomarasca et al., 2023
  mutate(
    Hc = if_else(is.na(Hc), Hc_b, Hc),
    LAImax = if_else(is.na(LAImax), LAImax_b, LAImax),
    SOURCE = if_else(is.na(SOURCE), SOURCE_b, SOURCE)
  ) %>% select(-Hc_b, -LAImax_b, -SOURCE_b) %>% 
  left_join(dat_flxn, by = "SITE_ID", suffix = c("", "_b")) %>% # (old) data from FLUXNET network
  mutate(
    Hc = if_else(is.na(Hc), Hc_b, Hc),
    LAImax = if_else(is.na(LAImax), LAImax_b, LAImax),
    SOURCE = if_else(is.na(SOURCE), SOURCE_b, SOURCE)
  ) %>% select(-Hc_b, -LAImax_b, -SOURCE_b) %>% 
  left_join(dat_lit, by = "SITE_ID", suffix = c("", "_b")) %>% # data found in the literature or on site websites
  mutate(
    Hc = if_else(is.na(Hc), Hc_b, Hc),
    LAImax = if_else(is.na(LAImax), LAImax_b, LAImax),
    SOURCE = if_else(is.na(SOURCE), SOURCE_b, SOURCE)
  ) %>% select(-Hc_b, -LAImax_b, -SOURCE_b) %>%
  ## MODIS LAI:
  left_join(modis_lai, by = "SITE_ID")
  # mutate(LAImax = if_else(is.na(LAImax), LAImax_modis, LAImax)) %>% # merge LAI columns, with priority on site estimates
  # select(-LAImax_modis)



### Assumptions on Vegetation Height (some grassland sites) --------------------
hc_sites_no_assumption <- dat %>% drop_na(Hc) %>% pull(SITE_ID);  length(hc_sites_no_assumption)

dat <- dat %>%
  ## Assumptions on Vegetation Height:
  left_join(read_csv(glue("data/input/igbp.csv"), show_col_types = F), by = "SITE_ID") %>% # add IGBP class
  mutate(
    Hc_flag = if_else(is.na(Hc) & IGBP %in% c("GRA", "WET"), "assumed", NA_character_), # flag sites where we assumed Hc = 0.5
    Hc = if_else(is.na(Hc) & IGBP %in% c("GRA", "WET"), 0.5, Hc)
    ) %>%
  ## Remove NAs:
  dplyr::filter(!(is.na(Hc) & is.na(LAImax))) %>%
  rename(SOURCE_LAImax = SOURCE) %>% 
  arrange(SITE_ID)

dat %>% dplyr::filter(Hc_flag == "assumed")
write_csv(dat, "data/input/veg_structure_assumedHc.csv") # extract sites with assumed Hc = 0.5
  
dat <- dat %>% dplyr::select(-Hc_flag)
  # %>% drop_na(LAImax) %>% select(SITE_ID, LAImax, LAImax_modis, SOURCE) # data 4 Reda


## Check sites with assumptions on Hc ----
load("data/input/species/species_cover_v05.RData")

dat_check <- read_csv("data/input/veg_structure_assumedHc.csv", show_col_types = F) %>%
  left_join(
    species_cover,
    by = c("SITE_ID", "IGBP")
    )
print("No species found for the subset of sites where no Hc was provided.")

rm(species_cover)


## Test difference to MODIS ----
# before adding MODIS LAI!
dat_diff <- inner_join(
  dat %>% select(SITE_ID, LAImax),
  modis_lai %>% select(SITE_ID, LAImax_modis),
  by = "SITE_ID"
) %>%
  drop_na()

## Correlation coefficient
cor_coeff <- cor(dat_diff$LAImax, dat_diff$LAImax_modis, method = "kendall")
# Kendall more robust with small sample sizes or outliers
cor_coeff

## Save correlation coefficient
if (savedata) {
  cat(
    glue::glue("Kendall’s tau = {round(cor_coeff, digits = 2)} over {nrow(dat_diff)} sites"),
    file = "results/correlation/LAImax_vs_MODIS-LAImax.txt"
  )
}

# p_lai <- dat_diff %>% # plot
#   ggplot(aes(LAImax, LAImax_modis)) +
#   geom_point(color = , size = , na.rm = T) +
#   geom_abline(slope = 1, color = "red3", linewidth = 0.75) +
#   lims(x = c(0, 10), y = c(0, 10)) +
#   theme_bw()
# p_lai

## Plot linear model with R2
p_lai <- plot_scatter_lm(dat_diff, LAImax, LAImax_modis) +
  geom_abline(slope = 1, color = line_color_plot, linewidth = 1, linetype = "dashed")
p_lai

if (savedata) { # save plot
  ggplot2::ggsave(filename = glue::glue("modisLAI_vs_siteLAI.jpg"), plot = p_lai, device = "jpeg",
                  path = "results/scatterplots", width = 508, height = 285.75, units = "mm", dpi = 100) # 1920 x  1080 px resolution (16:9)
}



## Check number of sites ----
hc_sites <- dat %>% drop_na(Hc) %>% pull(SITE_ID);  length(hc_sites)
lai_sites <- dat %>% drop_na(LAImax) %>% pull(SITE_ID)

miss_hc <- my_sites[!my_sites %in% hc_sites] # MISSING canopy height
miss_lai <- my_sites[!my_sites %in% lai_sites] # MISSING LAI

cat(paste0("==> ", length(hc_sites), " sites with canopy height and ", length(lai_sites), " sites with LAImax values.\n"),
    paste0("!=> Still missing ", length(miss_hc), " canopy height and ", length(miss_lai), " LAImax site values.\n")
    )

cat("Missing Hc:\n"); miss_hc
cat("Missing LAImax:\n"); miss_lai
# NB: probably good enough to assume Hc ~ 0 for wetlands, < 1 for grasslands etc...



### Vector of names ------------------------------------------------------------
struct_names <- dat %>% dplyr::select(-SITE_ID, -IGBP) %>% names() %>% sort()



### Save -----------------------------------------------------------------------
if (savedata) {
  write_csv(dat, "data/inter/veg_structure.csv") # data
  save(struct_names, file = glue::glue("data/inter/struct_names_{vers_out}.RData")) # vector of variable names
}