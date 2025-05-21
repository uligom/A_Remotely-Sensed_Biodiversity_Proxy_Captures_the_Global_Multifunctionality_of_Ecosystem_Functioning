### Function -------------------------------------------------------------------
old_calc_Gsmax <- function(data, site, year,
                       QCfilt = c(0, 1), GSfilt = 0.3, Pfilt = 0.1, Pfilt_time = 24, SWfilt = 200, USfilt = 0.2
)
{
  
  
  ## Utilities ----
  require(bigleaf)
  require(dplyr)
  require(tidyr)
  
  
  ## Quote & settings ----
  if (rlang::is_empty(year)) {
    site_year <- paste0("site ", site)
  } else if (!rlang::is_empty(year)) {
    data <- data %>% dplyr::mutate(YEAR = lubridate::year(DATETIME), .before = everything()) # add year
    
    year <- data %>% dplyr::pull(YEAR) %>% unique()
    site_year <- paste0("site-year ", site, "-", year)
  }
  
  
  ### Processing ----
  print(glue::glue("..computing Gsmax for {site_year}."))
  
  
  
  ## Filtering ----
  # Quality, Growing Season and Precipitation filter
  data <- bigleaf::filter.data(
    data.frame(data), quality.control = T, filter.growseas = T, filter.precip = T,
    GPP = "GPP", doy = "DOY", year = "YEAR", tGPP = GSfilt,
    precip = "Precip", tprecip = Pfilt, precip.hours = Pfilt_time, records.per.hour = 2,
    vars.qc = c("TA", "H", "LE", "NEE"), quality.ext = "_QC", good.quality = QCfilt) # missing "RH" QC filter
  
  # Radiation and u* filter
  data <- data %>% dplyr::filter(SW_IN > SWfilt & USTAR > USfilt)
  
  # Using only measured data: removing NA for wind speed
  data <- data %>% tidyr::drop_na(WS)
  
  # Convert units if needed
  if (!"VPD_kPa" %in% names(data)) {data <- data %>% dplyr::mutate(VPD_kPa = VPD / 10)} # convert units of VPD from [hPa] to [kPa]
  
  # Subset for calculations and omit NAs
  data_subset <- data %>% 
    dplyr::select(DATETIME, TA, PA, WS, USTAR, H, VPD_kPa, NETRAD, LE) %>% 
    tidyr::drop_na()
  
  
  ## Calculate EFPs ----
  if (nrow(data_subset) == 0) { # if no data is available after filtering
    stop(glue::glue("The {site_year} was skipped because of empty data."))
    output <- NA_real_
    
    
  } else if (nrow(data_subset) != 0) { # if dataframe is not empty
    
    
    output <- data_subset %>%
      ## Calculate aerodynamic conductance
      mutate(Ga = aerodynamic.conductance(., Tair = TA, pressure = PA,
                                          wind = WS, ustar = USTAR, H = H, 
                                          Rb_model = "Thom_1972") %>% 
               pull(Ga_h),
             ## Calculate surface conductance
             Gs = surface.conductance(., Ga = Ga, Tair = TA, pressure = PA, Rn = NETRAD,
                                      G = NULL, S = NULL, VPD = VPD_kPa, LE = LE,  
                                      missing.G.as.NA = F, missing.S.as.NA = F)
      ) %>%
      unnest(Gs) %>%
      ## Other filters
      tidyr::drop_na(Gs_mol) %>%
      dplyr::filter(VPD_kPa > 0) %>%  # VPD filter
      ## Calculate Gsmax (by years if specified) (?)
      summarise(Gsmax = quantile(na.omit(Gs_ms), 0.90), # 90th Gsmax quantile
                .groups = "drop") %>% 
      pull()
  } # end condition of empty data
  
  
  ### Output ----
  return(output)
}



# ### Debug ----------------------------------------------------------------------
# debugonce(old_calc_Gsmax)