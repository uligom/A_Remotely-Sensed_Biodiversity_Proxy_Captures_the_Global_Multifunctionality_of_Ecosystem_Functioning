### Function -------------------------------------------------------------------
old_import_data_and_calc_EFPs <- function(
    site_list = list("AR-SLu", "AT-Neu", "AU-Cpr"),
    path = "//minerva/BGI/scratch/jnelson/4Sinikka/data20240123/", path2 = "//minerva/BGI/work_1/scratch/fluxcom/sitecube_proc/model_files/",
    future_env = list(savedata, eval_file, grouping_var, QCfilt, GSfilt, Pfilt, Pfilt_time, SWfilt, USfilt, GPPsatfilt),
    plotting = F
) {
  
  
  ### Utilities ----------------------------------------------------------------
  require(dplyr)
  require(lubridate)
  require(ncdf4)
  require(stringr)
  require(tidyr)
  
  
  
  ### Function inputs ----------------------------------------------------------
  site <- unlist(site_list)       # site of interest (parallized, also works for testing)
  
  savedata <- future_env[[1]]     # save or not
  eval_file <- future_env[[2]]    # evaluation file
  grouping_var <- future_env[[3]] # calculate by year or for full site timeseries
  
  QCfilt <- future_env[[4]]       # quality filter
  GSfilt <- future_env[[5]]       # growing season
  Pfilt  <- future_env[[6]]       # precipitation filter
  Pfilt_time <- future_env[[7]]   # period to exclude after precipitation events (hours)
  SWfilt <- future_env[[8]]       # radiation filter (daytime)
  USfilt <- future_env[[9]]       # u* filter
  GPPsatfilt <- future_env[[10]]  # filter for GPPsat outliers
  
  
  
  # Initialize txt vector for evaluation
  txt_vector <- c()
  
  
  
  ## Output by errors ----
  err_output <- tibble(SITE_ID = site,
                       CUEeco90 = NA_real_, CUEpval90 = NA_real_,
                       GPPsat = NA_real_,
                       Gsmax = NA_real_, NEP95 = NA_real_, NEP99 = NA_real_,
                       uWUE = NA_real_, WUE = NA_real_
  ) # output when error is encountered
  
  
  
  ### Import data --------------------------------------------------------------
  ## Import data for the current site
  txt <- glue::glue("++++++++++++++++++++++++++++ Site {site} ++++++++++++++++++++++++++++"); print(txt); txt_vector <- c(txt_vector, txt)
  txt <- glue::glue("....Importing data for site {site}."); print(txt); txt_vector <- c(txt_vector, txt)
  
  ## Fluxes
  nc_fluxes <- tryCatch({
    ncdf4::nc_open(filename = glue::glue("{path}{site}.nc")) # fluxes
  }, error = function(err) {
    return(NA)
  })
  if (typeof(nc_fluxes) == "logical") { # condition to exit computations for current site
    txt <- glue::glue("!=> Error in the import of flux data for site {site}. Skipping current site."); warning(txt); txt_vector <- c(txt_vector, txt)
    if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
    
    return(err_output)
    rlang::interrupt()
  }
  
  nc_fluxes2 <- tryCatch({
    ncdf4::nc_open(filename = glue::glue("{path2}{site}_meteo.nc"))
  }, error = function(err) {
    return(NA)
  })
  if (typeof(nc_fluxes2) == "logical") { # condition to exit computations for current site
    txt <- glue::glue("!=> Error in the import of flux data2 for site {site}. Skipping current site."); warning(txt); txt_vector <- c(txt_vector, txt)
    if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
    
    return(err_output)
    rlang::interrupt()
  }
  
  # ## Remote sensing
  # nc_rs <- tryCatch({
  # ncdf4::nc_open(filename = glue::glue("{path2}{site}_rs.nc")) # remote sensing data
  # }, error = function(err) {
  #   return(NA)
  # })
  # if (typeof(nc_rs) == "logical") { # condition to exit computations for current site
  #   txt <- glue::glue("!=> Error in the import of remote sensing data for site {site}. Skipping current site."); warning(txt); txt_vector <- c(txt_vector, txt)
  #   if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
  #   
  #   return(err_output)
  #   rlang::interrupt()
  # }
  
  # Origin for time dimension
  timestart_fluxes <- stringr::str_extract(nc_fluxes[["dim"]][["time"]][["units"]], "[:digit:]{4}-[:digit:]{2}-[:digit:]{2} [:digit:]{2}:[:digit:]{2}:[:digit:]{2}")
  timestart_fluxes2 <- stringr::str_extract(nc_fluxes2[["dim"]][["time"]][["units"]], "[:digit:]{4}-[:digit:]{2}-[:digit:]{2} [:digit:]{2}:[:digit:]{2}:[:digit:]{2}")
  
  
  ## Extract variables ----
  dat <- tryCatch({
    tibble(
      ## Site:
      SITE_ID = site,
      ## Time:
      TIME = nc_fluxes$dim$time$vals, # time dimension (minutes since YYYY-MM-DD 00:00:00)
      ## Coordinates:
      LATITUDE = ncdf4::ncvar_get(nc_fluxes, varid = "tower_lat"),  # latitude (degrees north)
      LONGITUDE = ncdf4::ncvar_get(nc_fluxes, varid = "tower_lon"), # latitude (degrees east)
      ## Fluxes with quality flags:
      NEE = ncdf4::ncvar_get(nc_fluxes, varid = "NEE"),             # net ecosystem exchange (µmol m-2 s-1)
      NEE_QC = ncdf4::ncvar_get(nc_fluxes, varid = "NEE_QC"),       # net ecosystem exchange quality flag
      GPP = ncdf4::ncvar_get(nc_fluxes, varid = "GPP_NT"),          # gross primary productivity (µmol m-2 s-1)
      # GPP_QC = ncdf4::ncvar_get(nc_fluxes, varid = "GPP_QC"),       # gross primary productivity quality flag
      RECO = ncdf4::ncvar_get(nc_fluxes, varid = "RECO_NT"),        # ecosystem respiration (µmol m-2 s-1)
      # RECO_QC = ncdf4::ncvar_get(nc_fluxes, varid = "RECO_NT_QC"),  # ecosystem respiration quality flag
      H = ncdf4::ncvar_get(nc_fluxes, varid = "H"),                 # sensible heat flux (W m-2)
      H_QC = ncdf4::ncvar_get(nc_fluxes, varid = "H_QC"),           # sensible heat flux quality flag
      LE = ncdf4::ncvar_get(nc_fluxes, varid = "LE"),               # latent heat flux (W m-2)
      LE_QC = ncdf4::ncvar_get(nc_fluxes, varid = "LE_QC"),         # latent heat flux quality flag
      # G = ncdf4::ncvar_get(nc_fluxes, varid = "G"),                 # ground heat flux (W m-2)
      # G_QC = ncdf4::ncvar_get(nc_fluxes, varid = "G_QC"),           # ground heat flux quality flag
      ## Meteo with quality flags:
      NETRAD = ncdf4::ncvar_get(nc_fluxes, varid = "NETRAD"),       # net surface radiation (W m-2)
      NETRAD_QC = ncdf4::ncvar_get(nc_fluxes, varid = "NETRAD_QC"), # net radiation quality flag
      # LW_IN = ncdf4::ncvar_get(nc_fluxes, varid = "LW_IN"),         # downward long-wave radiation (W m-2)
      # LW_IN_QC = ncdf4::ncvar_get(nc_fluxes, varid = "LW_IN_QC"),   # downward long-wave radiation quality flag
      Precip = ncdf4::ncvar_get(nc_fluxes, varid = "P"),            # precipitation rate (mm h-1)
      Precip_QC = ncdf4::ncvar_get(nc_fluxes, varid = "P_QC"),      # precipitation rate quality flag
      PA = ncdf4::ncvar_get(nc_fluxes, varid = "PA"),               # surface pressure (Pa)
      PA_QC = ncdf4::ncvar_get(nc_fluxes, varid = "PA_QC"),         # surface pressure quality flag
      # RH = ncdf4::ncvar_get(nc_fluxes, varid = "RH"),               # relative humidity (%)
      # RH_QC = ncdf4::ncvar_get(nc_fluxes, varid = "RH_QC"),         # relative humidity quality flag
      SW_IN = ncdf4::ncvar_get(nc_fluxes, varid = "SW_IN"),         # downward short-wave radiation (W m-2)
      SW_IN_QC = ncdf4::ncvar_get(nc_fluxes, varid = "SW_IN_QC"),   # downward short-wave radiation quality flag
      TA = ncdf4::ncvar_get(nc_fluxes, varid = "TA"),               # near surface air temperature (K)
      TA_QC = ncdf4::ncvar_get(nc_fluxes, varid = "TA_QC"),         # near-surface air temperature quality flag
      USTAR = ncdf4::ncvar_get(nc_fluxes, varid = "USTAR"),         # friction velocity (m s-1)
      USTAR_QC = ncdf4::ncvar_get(nc_fluxes, varid = "USTAR_QC"),   # friction velocity quality flag
      VPD = ncdf4::ncvar_get(nc_fluxes, varid = "VPD"),             # vapor pressure deficit (hPa)
      VPD_QC = ncdf4::ncvar_get(nc_fluxes, varid = "VPD_QC"),       # vapor pressure deficit quality flag
      WS = ncdf4::ncvar_get(nc_fluxes, varid = "WS"),               # near surface wind speed (m s-1)
      WS_QC = ncdf4::ncvar_get(nc_fluxes, varid = "WS_QC")          # near surface wind speed quality flag
    ) %>%
      mutate(across(.cols = everything(), .fns = as.vector)) %>% # convert every column type (array) to vector
      mutate(across(.cols = where(is.double), .fns = ~ if_else(condition = is.nan(.x), true = NA_real_, false = .x))) %>% # convert NaN to NA
      mutate(TIME = TIME * 60, # convert time from 'minutes from' to 'seconds from'
             DATETIME = lubridate::as_datetime(TIME, origin = timestart_fluxes), # generate date column from correct start for each site
             .after = SITE_ID
      ) %>% 
      dplyr::left_join( # import missing variables from different file location
        tibble(
          SITE_ID = site,
          TIME = nc_fluxes2$dim$time$vals,                               # time dimension (minutes since YYYY-MM-DD 00:00:00)
          IGBP = ncdf4::ncvar_get(nc_fluxes2, varid = "IGBP_veg_short"), # IGBP plant functional type classification
          RH = ncdf4::ncvar_get(nc_fluxes2, varid = "RH"),               # relative humidity (%)
          # RH_QC = ncdf4::ncvar_get(nc_fluxes, varid = "RH_QC"),          # relative humidity quality flag
        ) %>%
          mutate(across(.cols = everything(), .fns = as.vector)) %>% # convert every column type (array) to vector
          mutate(across(.cols = where(is.double), .fns = ~ if_else(condition = is.nan(.x), true = NA_real_, false = .x))) %>% # convert NaN to NA
          mutate(TIME = TIME * 60, DATETIME = as_datetime(TIME, origin = timestart_fluxes2)) %>% # generate date column from correct start for each site
          dplyr::select(-TIME), # remove time for to-be-joined tibble to avoid confusion
        by = c("SITE_ID", "DATETIME")
      ) %>% dplyr::relocate(IGBP, .after = LONGITUDE) %>% dplyr::relocate(RH, .after = PA_QC)
    
  }, error = function(err) {
    return(NA)
  })
  if (typeof(dat) == "logical") { # condition to exit computations for current site
    txt <- glue::glue("!=> Error in the extraction of variables for site {site}. Skipping current site."); warning(txt); txt_vector <- c(txt_vector, txt)
    if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
    
    return(err_output)
    rlang::interrupt()
  }
  
  ## Check empty data
  if (nrow(dat) == 0) {
    txt <- glue::glue("!=> No valid data available for site {site}. Skipping current site.")
    print(txt); txt_vector <- c(txt_vector, txt)
    if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
    
    return(err_output)
    rlang::interrupt()
  }
  
  
  ## Convert units ----
  txt <- glue::glue("....Converting units for site {site}."); print(txt); txt_vector <- c(txt_vector, txt)
  
  dat <- tryCatch({dat %>%
      dplyr::mutate(VPD_kPa = VPD / 10, .after = VPD) %>% # convert units of VPD from [hPa] to [kPa]
      dplyr::mutate(
        TA = dplyr::if_else(TA < -150, true = TA + 273.15, false = TA), # from [K] to [°C], if necessary
        # NEE = NEE * 1e+03 / 44.009 * 1e+06,   # from [kg m-2 s-1] to [µmol m-2 s-1] ==> # 1 kg = 1e+03 g;  1 g CO2 = 1/44.009 mol CO2;  1 mol = 1e+06 µmol
        # GPP = GPP * 1e+03 / 44.009 * 1e+06,   # from [kg m-2 s-1] to [µmol m-2 s-1]
        # RECO = RECO * 1e+03 / 44.009 * 1e+06, # from [kg m-2 s-1] to [µmol m-2 s-1]
        Precip = Precip / 60^2 # from [mm h-1] to [mm s-1]
      )
  }, error = function(err) {
    return(NA)
  })
  if (typeof(dat) == "logical") { # condition to exit computations for current site
    txt <- glue::glue("!=> Error in the units conversion for {site}. Skipping current site."); warning(txt); txt_vector <- c(txt_vector, txt)
    if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
    
    return(err_output)
    rlang::interrupt()
  }
  
  
  
  ## Calculate missing variables ----
  txt <- glue::glue("....Calculating missing variables for site {site}."); print(txt); txt_vector <- c(txt_vector, txt)
  
  dat <- tryCatch({dat %>%
      ## Time:
      dplyr::mutate(YEAR = year(DATETIME),
                    DOY = yday(DATETIME),
                    HOUR_decimal = (hour(DATETIME) * 60 + minute(DATETIME) + second(DATETIME)) / 60, # extract hours, minutes & seconds, and convert to decimal hour
                    .after = DATETIME
      ) %>%
      ## Fluxes:
      dplyr::mutate(ET = REddyProc::fCalcETfromLE(LE = LE, Tair = TA), # evapotranspiration [mmol H20 m-2 s-1]
      ) %>%
      ## Climate:
      dplyr::mutate(PAR = SW_IN * 2.11, # calculation of PAR [umol m^-2 s^-1] from SW_IN [W m^-2]
                    PPFD = SW_IN * 2.11, # photosynthetic photon flux density (PPFD) calculated as SW_IN * 2.11
                    SW_IN_POT = fCalcPotRadiation(DoY = DOY, Hour = HOUR_decimal,
                                                  LatDeg = LATITUDE, LongDeg = LONGITUDE, TimeZone = 0) # Timezone is UTC for all sites for the NEON-NCAR product (0)
      ) %>% 
      ## Quality flags:
      dplyr::mutate(GPP_QC = NEE_QC, RECO_QC = NEE_QC) %>% 
      ## Moving window
      dplyr::mutate(FiveDaySeq = rep(c(1:ceiling(n()/5)), each = 48 * 5, length.out = n()))
  }, error = function(err) {
    return(NA)
  })
  if (typeof(dat) == "logical") { # condition to exit computations for current site
    txt <- glue::glue("!=> Error in the calculation of missing variables for {site}. Skipping current site."); warning(txt); txt_vector <- c(txt_vector, txt)
    if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
    
    return(err_output)
    rlang::interrupt()
  }
  
  
  
  ### Plot variable timeseries -------------------------------------------------
  if (plotting) {
    if (savedata) {savepath <- "results/timeseries"} else {savepath <- NA}
    
    dat %>% plot_timeseries(y = "ET", color = "LE_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "GPP", color = "GPP_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "H", color = "H_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "LE", color = "LE_QC", site = site, savepath = savepath)
    # dat %>% plot_timeseries(y = "LW_IN", color = "LW_IN_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "NEE", color = "NEE_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "NETRAD", color = "NETRAD_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "PA", color = "PA_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "Precip", color = "Precip_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "RECO", color = "GPP_QC", site = site, savepath = savepath)
    # dat %>% plot_timeseries(y = "RH", color = "RH_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "SW_IN", color = "SW_IN_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "SW_IN_POT", color = "SW_IN_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "TA", color = "TA_QC", site = site, savepath = savepath)
    # dat %>% plot_timeseries(y = "USTAR", color = "USTAR_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "VPD", color = "VPD_QC", site = site, savepath = savepath)
    dat %>% plot_timeseries(y = "WS", color = "WS_QC", site = site, savepath = savepath)
  }
  
  
  
  ### Filtering ----------------------------------------------------------------
  ## Exclude cropland sites ----
  if (unique(dat$IGBP) == "CRO") {
    txt <- glue::glue("....Excluding cropland sites. Site {site} was excluded."); print(txt); txt_vector <- c(txt_vector, txt)
    
    return(err_output)
    rlang::interrupt()
  }
  
  
  
  ### EFP calculation ----------------------------------------------------------
  ## Announce computation
  if (rlang::is_empty(as.character(grouping_var))) {
    txt <- glue::glue("....Computing EFPs for site {site}.")
    print(txt); txt_vector <- c(txt_vector, txt)
  } else if (!rlang::is_empty(as.character(grouping_var))) {
    txt <- glue::glue("....Computing EFPs for each year for site {site}.")
    print(txt); txt_vector <- c(txt_vector, txt)
  }
  
  
  ## Compute ----
  tic(glue::glue("Time to calculate EFPs for site {site}"))
  dat_out <- tryCatch({dat %>%
      dplyr::group_by(!!grouping_var) %>% # grouping variables for mapping
      tidyr::nest(data4EFPs = -c(SITE_ID, !!grouping_var)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        ## CUEeco
        CUEeco = purrr::map(
          .x = data4EFPs, .f = old_calc_CUEeco,
          site = site, year = as.character(grouping_var),
          qile = 0.9,
          QCfilt = QCfilt, GSfilt = GSfilt, Pfilt = Pfilt, Pfilt_time = Pfilt_time, SWfilt = SWfilt, USfilt = USfilt
        ),
        ## GPPsat & NEP95
        LRCmetrics = purrr::map(
          .x = data4EFPs, .f = old_calc_GPPsat_NEPmax,
          site = site, year = as.character(grouping_var),
          QCfilt = QCfilt, GSfilt = GSfilt, SWfilt = SWfilt, USfilt = USfilt, GPPsatfilt = GPPsatfilt
        ),
        ## Gsmax
        Gsmax = purrr::map_dbl(
          .x = data4EFPs, .f = old_calc_Gsmax,
          site = site, year = as.character(grouping_var),
          QCfilt = QCfilt, GSfilt = GSfilt, Pfilt = Pfilt, Pfilt_time = Pfilt_time, SWfilt = SWfilt, USfilt = USfilt
        ),
        ## WUE
        WUEmetrics = purrr::map(
          .x = data4EFPs, .f = old_calc_WUE,
          site = site, year = as.character(grouping_var),
          QCfilt = QCfilt, GSfilt = GSfilt, Pfilt = Pfilt, Pfilt_time = Pfilt_time, SWfilt = SWfilt, USfilt = USfilt
        )
        # ## WUEt
        # WUEt = purrr::map_dbl(.x = data4EFPs, .f = calc_WUEt,
        #                site = SITE_ID, year = !!grouping_var,
        #                QCfilt = QCfilt, GSfilt = GSfilt, Pfilt = Pfilt, Pfilt_time = Pfilt_time, SWfilt = SWfilt * 2, USfilt = USfilt
        # )
      ) %>%
      tidyr::unnest(cols = c(CUEeco, LRCmetrics, WUEmetrics)) %>% # extract variables; to keep empty outputs: 'keep_empty = T'
      dplyr::select(-data4EFPs) # remove input data
    
  }, error = function(err) {
    return(NA)
  })
  if (typeof(dat_out) == "logical") { # condition to exit computations for current site
    txt <- glue::glue("!=> Error in the EFP calculations for site {site}. Skipping current site."); warning(txt); txt_vector <- c(txt_vector, txt)
    if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
    
    return(err_output)
    rlang::interrupt()
  }
  
  toc()
  
  
  
  ### Clean memory -------------------------------------------------------------
  rm(nc_fluxes, nc_fluxes2, dat, timestart_fluxes, timestart_fluxes2)
  gc() # clean memory usage
  
  
  
  ### Output -------------------------------------------------------------------
  txt <- glue::glue("==> EFP calculations performed correctly for site {site}.")
  print(txt); txt_vector <- c(txt_vector, txt)
  if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
  
  return(dat_out)
  
  
  
} # end of function



### Debugging ------------------------------------------------------------------
debugonce(old_import_data_and_calc_EFPs)