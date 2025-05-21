#### Function 
calc_phenodiv <- function(
    data = NULL, site = "SITE_ID", longitude = "longitude", latitude = "latitude", data_path = "",
    future_env = list(product, radius, avail_abs, avail_rel, n_removal, elong, yearly, savedata, save_string),
    transparent_plots = F
) {
  ### Utilities ----------------------------------------------------------------
  require(terra)    # raster manipulation
  require(zoo)      # roll means
  
  
  
  #### Function input ####
  ### Arguments to values ----------------------------------------------------
  site <- data %>% dplyr::pull(.data[[site]])
  longitude = data %>% dplyr::pull(.data[[longitude]])
  latitude = data %>% dplyr::pull(.data[[latitude]])
  # site <- SITES %>% dplyr::filter(SITE_ID == "DE-Tha") %>% pull(SITE_ID) # for testing
  # longitude <- SITES %>% dplyr::filter(SITE_ID == "DE-Tha") %>% pull(longitude) # for testing
  # latitude <- SITES %>% dplyr::filter(SITE_ID == "DE-Tha") %>% pull(latitude) # for testing
  
  radius      <- future_env[[1]]
  avail_abs   <- future_env[[2]]
  avail_rel   <- future_env[[3]]
  n_removal   <- future_env[[4]]
  elong       <- future_env[[5]]
  yearly      <- future_env[[6]]
  savedata    <- future_env[[7]]
  save_string <- future_env[[8]]
  
  rm(future_env)
  
  ## Output by errors ----
  err_output <- tibble(SITE_ID = site, phe_std_mean = NA_real_, phe_std_max = NA_real_,
                       phe_std_int = NA_real_, phe_var = list(NA)) # output when error is encountered
  
  
  #### Main function ####
  tryCatch({
    ### Load data --------------------------------------------------------------
    cat("\n=== === === === === Calculations for site", site, "=== === === === ===\n")
    
    ## NDVI
    # NDVI = (B8 − B4) / (B8 + B4) # B8 = NIR, B4 = red
    B4 <- rast(x = glue::glue("{data_path}{site}_S2_2A_bandB4.tif"))
    B8 <- rast(x = glue::glue("{data_path}{site}_S2_2A_bandB8.tif"))
    
    NDVIrast <- rast(x = glue::glue("{data_path}S2_{product}_NDVI_{site}.tif"))
    # NDVIstars <- read_stars(glue::glue("{data_path}S2_{product}_NDVI_{site}.tif")) %>% # needed for plotting
    #   setNames("NDVI") %>% 
    #   st_set_dimensions(names = c("longitude", "latitude", "timestamp"))
    
    ## Date
    DOYrast <- rast(glue::glue("{data_path}S2_{product}_DOY_{site}.tif"))
    YEARrast <- rast(glue::glue("{data_path}S2_{product}_YEAR_{site}.tif"))
    
    
    
    ### Convert dates ----------------------------------------------------------
    ## Sentinel 2
    DOYvec <- DOYrast[1,1] %>% unlist() %>% unname()    # day of year vector
    YEARvec <- YEARrast[1,1] %>% unlist() %>% unname()  # year vector
    DATES <- strptime(paste(YEARvec, DOYvec), format = "%Y %j") %>% # dates vector
      as.character() %>% as.POSIXct()
    
    
    ## Clean memory
    rm(DOYrast, YEARrast) # remove unnecessary data
    gc() # clean memory usage
    
    
    
    ### Region of interest -------------------------------------------------------
    circle <- sf::st_buffer(x = sf::st_sfc(sf::st_point(x = c(longitude, latitude)), crs = "EPSG:4326"), dist = radius) # circular region of interest of radius 'radius'
    
    
    
    ## Plotting colors ----
    purple_colblind <- "#B22D6E"
    gold_colblind <- "#FDB71E"
    teal_colblind <- "#1EB8B3"
    orange_colblind <- "#F15620"
    myCol <- c("#B22D6E", "#F15620", "#FDB71E", "#73C03C", "#1EB8B3", "#3C368E")
    
    
    
    # ### Plot spatial -------------------------------------------------------------
    # ## Data for plotting
    # idt <- runif(36, min = 1, max = dim(NDVIstars)[3]) %>% as.integer()
    # dat_plot <- NDVIstars %>%
    #   slice(index = idt, along = "timestamp") # randomly subset timestamps
    # 
    # ## Labels
    # date_labs <- DATES[idt] %>% as.character()
    # names(date_labs) <- idt %>% as.character()
    # 
    # ## Plot
    # p_space <- plot_sat_roi(data = dat_plot, timestamp = timestamp, roi = circle, labels = date_labs)
    # 
    # 
    # ## Save
    # if (savedata == T) {
    #   ggsave(filename = glue::glue("results/S2_NDVI/cutouts_{site}_{save_string}_{vers_out}.jpg"),
    #          plot = p_space, device = "jpeg",
    #          width = 400, height = 300, units = "mm", dpi = 300)
    # }
    # # ggsave(filename = glue::glue("results/S2_NDVI/cutouts1_{site}_r{radius}m_{vers_out}.png"),
    # #        plot = p_space, device = "jpeg", bg = "transparent",
    # #        width = 400, height = 300, units = "mm", dpi = 300)
    # 
    # 
    
    ### Cut-out ------------------------------------------------------------------
    circle_vec <- terra::vect(circle) # convert to SpatVector
    
    dates <- DATES %>% tibble::as_tibble_col(column_name = "date") %>% # convert date vector to dataframe
      mutate(cell_id = seq(from = 1, to = length(DATES))) # add ID for joining
    
    # dates_modis <- DATESmodis %>% tibble::as_tibble_col(column_name = "date") %>% # convert date vector to dataframe
    #   mutate(cell_id = seq(from = 1, to = length(DATESmodis))) # add ID for joining
    
    
    
    ## Crop data region ----
    ## Sentinel 2
    dat <- terra::crop(NDVIrast, circle_vec, mask = T) %>% # select cut-out region
      terra::as.data.frame(cells = T, xy = T, na.rm = F) %>%
      tidyr::pivot_longer(cols = !c(cell, x, y), names_to = "name", values_to = "NDVIs2") %>%
      dplyr::rename(longitude = x, latitude = y) %>%
      group_by(name) %>%
      mutate(cell_id = if_else(str_detect(name, "S2_"),
                               true = str_extract(name, "[:digit:]+$") %>% as.integer(), # names of bands in the form "S2_1C_NDVI_AU-Wom_1"
                               false = cur_group_id() # names of bands in the form "20160119T104352_20160119T104347_T31UGR_20160119T104352_20160119T104347_T31UGR_ndvi"
      )
      ) %>%
      left_join(dates, by = "cell_id") %>% # add dates
      ungroup() %>%
      dplyr::select(-name) %>%
      mutate(DOY = lubridate::yday(date) %>% as.integer(), # add DOY
             YEAR = lubridate::year(date) %>% as.integer(), # add YEAR
             NDVIs2 = ifelse(is.nan(NDVIs2), NA_real_, NDVIs2), # convert NaN to NA
             cell = cell %>% as.integer() # convert to integer to save memory
      )
    
    # Remove pixels with no data (i.e. outside circle), since terra::crop keeps rectangular shape
    dat <- dat %>%
      group_by(cell) %>% # by pixel
      mutate(non_na = sum(!is.na(NDVIs2))) %>% # count non-NA entries
      ungroup() %>%
      filter(non_na > 0) %>% # exclude pixels that have no data (corners outside circle)
      select(-non_na)
    
    # ## MODIS
    # dat_modis <- terra::crop(SNOWrast, circle_vec, mask = T) %>% # select cut-out region
    #   terra::as.data.frame(cells = T, xy = T, na.rm = F) %>%
    #   tidyr::pivot_longer(cols = !c(cell, x, y), names_to = "name", values_to = "SNOW") %>%
    #   dplyr::rename(longitude = x, latitude = y) %>%
    #   group_by(name) %>%
    #   mutate(cell_id = if_else(str_detect(name, "MODIS_"),
    #                            true = str_extract(name, "[:digit:]+$") %>% as.integer(), # names of bands in the form "S2_1C_NDVI_AU-Wom_1"
    #                            false = cur_group_id() # names of bands in the form "20160119T104352_20160119T104347_T31UGR_20160119T104352_20160119T104347_T31UGR_ndvi"
    #   )
    #   ) %>%
    #   left_join(dates_modis, by = "cell_id") %>% # add dates
    #   ungroup() %>%
    #   dplyr::select(-name) %>%
    #   mutate(DOY = lubridate::yday(date) %>% as.integer(), # add DOY
    #          YEAR = lubridate::year(date) %>% as.integer(), # add YEAR
    #          # SNOW = SNOW %>% as.integer(), # convert to integer to save memory
    #          cell = cell %>% as.integer()  # convert to integer to save memory
    #   )
    #
    #
    ## Condition to skip site if no data is available ----
    if (nrow(dat) == 0) {
      warning(paste("Site", site, "was skipped because of empty Sentinel 2 cut-outs."))
      return(err_output)
      next
    }
    
    ## Clear memory ----
    rm(NDVIrast)
    gc()
    
    
    # ## Plot check ----
    # idt <- runif(36, min = 1, max = dim(NDVIstars)[3]) %>% as.integer()
    # dat_plot <- dat %>%
    #   filter(cell_id %in% idt) %>% # randomly subset timestamps
    #   dplyr::select(-cell_id, -cell) %>%
    #   as.data.frame() %>%
    #   st_as_stars(dims = c("longitude", "latitude", "date")) # convert to stars object for plotting
    # 
    # plot_sat_roi(dat_plot, timestamp = date, roi = circle) # plot
    #
    #
    # ## Clean memory
    # rm(NDVIstars) # remove unnecessary data
    # gc() # clean memory usage
    #
    #
    ### Remove duplicate entries and superfluous information ----------------------
    dat <- dat %>% 
      select(-cell_id, -longitude, -latitude) %>% # NB1: removing duplicate cells (should not be necessary with updated GEE script)
      # NB2: removing coordinates of single cells (not needed in this script)
      unique() # remove unexplained duplicate entries (e.g. year 2016)
    
    
    
    # ### Plot temporal ------------------------------------------------------------
    # ## Data for plotting
    # set.seed(6314)
    # idx <- runif(36, min = 1, max = max(dat_cut$cell)) %>% as.integer()
    # idx
    # 
    # ## Data
    # dat_plot <- dat_cut %>%
    #   dplyr::filter(cell %in% idx) # randomly subset pixel
    # 
    # ## Plot data extent
    # dat_idx <- dat_plot %>% 
    #   group_by(cell) %>% 
    #   summarize(NDVImean = mean(NDVI, na.rm = T),
    #             longitude = x %>% unique(),
    #             latitude = y %>% unique()) %>%
    #   dplyr::select(-cell) %>% 
    #   as.data.frame() %>%
    #   st_as_stars(dims = c("longitude", "latitude")) # convert to stars object for plotting
    # 
    # p_idx <- ggplot() +
    #   stars::geom_stars(data = dat_idx) + # plot random subsets
    #   scale_fill_distiller(palette = "Spectral", direction = 1, na.value = "white") +
    #   geom_sf(data = circle, show.legend = "polygon", inherit.aes = F,
    #           color = "black", fill = NA, size = 1) + # plot region of interest
    #   geom_sf(data = point0, show.legend = "point", shape = 2, size = 7, stroke = 2) +
    #   labs(title = glue::glue("Random pixels ({site}).")) +
    #   theme_classic() +
    #   theme(axis.text = element_text(size = 24),
    #         axis.text.x = element_text(angle = 270),
    #         title = element_text(size = 32)) +
    #   NULL
    # 
    # ## Labels
    # pix_labs <- paste0("pixel ", dat_plot$cell %>% unique())
    # names(pix_labs) <- dat_plot$cell %>% unique()
    # 
    # ## Plot
    # p_date <- plot_sat_time(data = dat_plot, timestamp = date, y = NDVI, labels = pix_labs)
    # p_doy <- plot_sat_time(data = dat_plot, timestamp = doy, y = NDVI, labels = pix_labs)
    # p_agg <- dat_plot %>% 
    #   group_by(cell, doy) %>% 
    #   mutate(NDVIdoy = mean(NDVI, na.rm = T)) %>% 
    #   plot_sat_time(timestamp = doy, y = NDVIdoy, labels = pix_labs)
    # 
    # ## Save
    # if (savedata == T) {
    #   ggsave(filename = glue::glue("results/S2_NDVI/randpix4timeseries_{site}_r{radius}m_{vers_out}.jpg"),
    #          plot = p_idx, device = "jpeg",
    #          width = 400, height = 300, units = "mm", dpi = 300)
    #   ggsave(filename = glue::glue("results/S2_NDVI/timeseries_date_{site}_r{radius}m_{vers_out}.jpg"),
    #          plot = p_date, device = "jpeg",
    #          width = 500, height = 281.25, units = "mm", dpi = 300)
    #   ggsave(filename = glue::glue("results/S2_NDVI/timeseries_doy_{site}_r{radius}m_{vers_out}.jpg"),
    #          plot = p_doy, device = "jpeg",
    #          width = 500, height = 281.25, units = "mm", dpi = 300)
    #   ggsave(filename = glue::glue("results/S2_NDVI/timeseries_mean_{site}_r{radius}m_{vers_out}.jpg"),
    #          plot = p_agg, device = "jpeg",
    #          width = 500, height = 281.25, units = "mm", dpi = 300)
    # }
    # 
    # 
    # 
    ### Data availability filter -------------------------------------------------
    print(glue::glue("==> Applyng minimum data availability filter."))
    n_dat <- dat %>% nrow() # number of data entries before filter
    
    dat <- dat %>%
      # by date, to exclude low data availability timesteps
      group_by(date) %>%
      mutate(n_group = n(),                # count number of entries per group
             non_na = sum(!is.na(NDVIs2)), # count non-NA entries
             dat_avail = non_na / n_group  # relative data availability [0-1]
      ) %>%
      ungroup() %>%
      filter(dat_avail > avail_rel & non_na >= avail_abs) %>%
      # by pixel, to exclude ~empty ones
      group_by(cell, YEAR) %>%
      mutate(n_group = n(),                # count number of entries per group
             non_na = sum(!is.na(NDVIs2)), # count non-NA entries
             dat_avail = non_na / n_group  # relative data availability [0-1]
      ) %>%
      ungroup() %>%
      filter(non_na >= avail_abs) %>%
      # by year, to exclude low data availability years
      group_by(cell, YEAR) %>%
      mutate(n_group = n(),                # count number of entries per group
             non_na = sum(!is.na(NDVIs2)), # count non-NA entries
             dat_avail = non_na / n_group  # relative data availability [0-1]
      ) %>%
      ungroup() %>%
      filter(dat_avail > avail_rel & non_na >= avail_abs) %>%
      select(-n_group, -non_na, -dat_avail)
    
    
    ## Report result
    n_filt <- dat %>% nrow() # number of data entries after filter
    n_rem <- n_dat - n_filt
    perc_rem <- round(n_rem * 100 / n_dat, digits = 2)
    print(glue::glue("{n_rem} out of {n_dat} entries removed ({perc_rem} %)."))
    
    
    ### Remove negative NDVI values --------------------------------------------
    print(glue::glue("==> Removing remaining negative NDVI values."))
    n_dat <- dat %>% nrow() # number of data entries before filter
    
    dat <- dat %>% dplyr::filter(NDVIs2 >= 0 | is.na(NDVIs2))
    
    ## Report result
    n_filt <- dat %>% nrow() # number of data entries after filter
    n_rem <- n_dat - n_filt
    perc_rem <- round(n_rem * 100 / n_dat, digits = 2)
    print(glue::glue("{n_rem} out of {n_dat} entries removed ({perc_rem} %)."))
    
    
    
    ### Snow filter ??? --------------------------------------------------------
    # Whole-site filter based on MODIS snow
    
    #### Coarse spline + outlier removal loop ----------------------------------
    dat <- dat %>%
      mutate(outlier = F) %>% # initialize outlier flag
      dplyr::rename(NDVI = NDVIs2) # rename for ease
    
    
    ## Define spline window ----
    sos = 1
    eos = 366
    
    
    ### Loop for outlier removal ----
    for (sss in 1:n_removal) {
      ## Copy s2 data ----
      dat_s2 <- dat %>%
        select(cell, NDVI, date, DOY, YEAR, outlier) %>% 
        drop_na() %>% 
        unique()
      
      
      ### Coarse spline ----------------------------------------------------------
      print(glue::glue("==> Fitting spline on yearly data with {sss - 1} round(s) of outlier removal."))
      
      
      ## "Trick" spline with data elongation ----
      # before first year and after last year
      if (elong == T) {
        dat <- bind_rows(
          dat %>% filter(YEAR == min(YEAR)) %>% # duplicate data before first year
            mutate(YEAR = min(YEAR) - 1,
                   date = paste(YEAR, month(date), mday(date)) %>% as.Date(format = "%Y%m%d")
            ), 
          dat, # actual data
          dat %>% filter(YEAR == max(YEAR)) %>% # duplicate data after last year
            mutate(YEAR = max(YEAR) + 1,
                   date = paste(YEAR, month(date), mday(date)) %>% as.Date(format = "%Y%m%d")
            ),
        )
      } # end 'elongation trick' if sentence
      
      # ## Plot time-series with spline(s) ----
      # minyear <- paste((dat %>% select(YEAR) %>% min() + 1), "01 01") %>% as.Date(format = "%Y%m%d") %>% as.POSIXct() # start of real data
      # maxyear <- paste((dat %>% select(YEAR) %>% max() - 1), "12 31") %>% as.Date(format = "%Y%m%d") %>% as.POSIXct() # end of real data
      # 
      # if (sss == 1) { # first graph
      #   p_series <- ggplot() +
      #     geom_point(data = dat, aes(x = date, y = NDVI), size = 0.2, shape = ".") +
      #     geom_vline(xintercept = c(minyear, maxyear), color = "red3", linetype = "dashed", size = 0.5) +
      #     theme_classic() +
      #     NULL
      # } else { # subsequent graphs
      #   p_series <- ggplot() +
      #     geom_point(data = dat, aes(x = DOY, y = NDVI, color = outlier), size = 0.2, shape = ".") +
      #     facet_wrap(~ YEAR, scales = "free_x") +
      #     scale_color_discrete(direction = -1) +
      #     theme_classic() +
      #     NULL
      # }
      # 
      # 
      ## Fit spline over group (pixels + 3-years window) ----
      YEARS <- dat %>% pull(YEAR) %>% unique()
      
      dat_splin <- tibble::tibble() # initialize
      
      for (yyy in (2:(length(YEARS) - 1))) { # loop based on years to identify moving windows of 3 years
        year_window <- YEARS[(yyy-1):(yyy+1)] # moving window (3 years)
        dat_splin <- bind_rows(
          dat_splin,
          dat_s2 %>%
            filter(YEAR %in% year_window) %>% # filter moving window
            mutate(DOY = case_when(
              YEAR == year_window[1] ~ DOY - 366, # previous year
              YEAR == year_window[2] ~ DOY + 0,   # actual year
              YEAR == year_window[3] ~ DOY + 366  # next year
            )) %>% 
            group_by(cell) %>% 
            nest(s2_data = c(NDVI, date, YEAR, DOY, outlier)) %>%
            mutate("spline{sss}" := map(.x = s2_data, .f = tidy_spline, x = "DOY", y = "NDVI",
                                        filt = "outlier", dfden = 10, sos = sos, eos = eos) # fit spline
            ) %>%
            select(-s2_data) %>% # remove s2 data! (comment to see full spline input)
            mutate(YEAR = year_window[2], .after = cell) %>%
            ungroup()
        )
      } # end '3-years moving window' loop
      # TO DO: add reference spline when datapoints are low?
      
      
      ## Quote ----
      spline_sym <- glue::glue("spline{sss}") %>% sym()
      ndvi_sym <- glue::glue("NDVI{spline_sym}") %>% sym()  # symbol of splined NDVI output
      
      
      ## Remove empty output ----
      dat_splin <- dat_splin %>% drop_na(!!spline_sym)
      
      
      ## Merge all data back to dataframe ----
      dat <- dat_splin %>% 
        unnest(cols = !!spline_sym) %>% 
        dplyr::rename("{ndvi_sym}" := NDVI) %>% 
        left_join(dat, by = c("cell", "YEAR", "DOY"))
      
      
      ## Set negative values to 0 ----
      dat <- dat %>% mutate("{ndvi_sym}" := if_else(!!ndvi_sym < 0, 0, !!ndvi_sym))
      
      
      
      ### Outlier removal --------------------------------------------------------
      print(glue::glue("==> Round {sss} of outlier removal."))
      n_dat <- dat %>% nrow() # number of data entries before filter
      
      ## Calculate residuals ----
      dat <- dat %>% 
        mutate(resids = NDVI - !!ndvi_sym) %>% # residual = observed y – predicted y
        group_by(YEAR) %>% 
        mutate(mean_resid = mean(resids, na.rm = T), # mean of residuals
               std_resid = sd(resids, na.rm = T) # standard deviation of residuals
        ) %>% 
        ungroup()
      
      
      ## Flag outliers ----
      # outside range +2/-3 std of residuals from mean
      dat <- dat %>% 
        mutate(outlier = if_else(
          condition = resids > mean_resid + 2 * std_resid |
            resids < mean_resid - 3 * std_resid,
          true = T,
          false = outlier
        ))
      
      dat_s2 <- dat_s2 %>%
        left_join(dat %>% select(cell, DOY, YEAR, outlier) %>% mutate(YEAR = YEAR %>% as.integer()), 
                  by = c("cell", "DOY", "YEAR"), suffix = c("_old", "")) %>% 
        select(-outlier_old) %>% 
        unique()
      
      
      ## Report result
      n_filt <- dat %>% dplyr::filter(outlier == F | is.na(outlier)) %>% nrow() # number of data entries after filter
      n_rem <- n_dat - n_filt
      perc_rem <- round(n_rem * 100 / n_dat, digits = 2)
      print(glue::glue("A total of {n_rem} out of {n_dat} entries flagged as outliers ({perc_rem} %)."))
      
      
      # ## Plot time-series with splines and outliers ----
      # if (sss != 1) {
      #   p_series <- p_series +
      #     geom_line(data = dat, aes(x = DOY, y = !!ndvi_sym, color = myCol[sss]))
      # }
      
      
    } # end of n_removal loop
    
    
    ## Clean data ----
    dat <- dat %>%
      select(cell, YEAR, DOY, NDVI, date, outlier) %>% 
      drop_na()
    
    
    ## Clear memory ----
    rm(dat_s2)
    gc()
    
    
    
    ### Spline on all data -------------------------------------------------------
    dat_splin <- tibble::tibble() # initialize
    
    
    ## Define spline window ----
    sos = 1
    eos = 366
    
    
    ## Elongation + spline ----
    if (yearly == F) {
      print(glue::glue("==> Fit final spline on all data."))
      
      sym_year <- NULL # quote for later merge
      char_year <- NULL
      
      ## "Trick" spline with data elongation ----
      # repeat all data three times
      if (elong == T) {
        dat <- bind_rows(dat %>% mutate(series = -1), # replicated before
                         dat %>% mutate(series = 0),
                         dat %>% mutate(series = 1) # replicated after
        )
      } # end 'elongation trick' if sentence
      
      
      ## Fit spline to all years ----
      dat_splin <- dat %>%
        drop_na() %>% # should not be needed but just to make sure
        mutate(DOY = case_when( # add artificial doy for years before and after times-series
          series == -1 ~ DOY - eos, # year before time-series
          series == 0  ~ DOY + 0,   # actual time-series
          series == 1  ~ DOY + eos  # year after time-series
        )) %>% 
        group_by(cell) %>% 
        nest(s2_data = c(NDVI, date, YEAR, DOY, outlier)) %>% # nest necessary information for spline
        mutate(spline_out = map(.x = s2_data, .f = tidy_spline, x = "DOY", y = "NDVI",
                                filt = "outlier", dfden = 10, sos = sos, eos = eos) # fit spline
        ) %>%
        select(-s2_data) %>% # remove s2 data! (comment to see full spline input)
        ungroup()
      # TO DO: add reference spline when datapoints are low?
      
      ## Remove elongated and empty output
      dat_splin <- dat_splin %>% # from spline output data
        dplyr::filter(series == 0) %>%
        select(-series) %>%
        drop_na(spline_out)
      
      dat <- dat %>% # from input data
        dplyr::filter(series == 0) %>%
        select(-series)
      
      
    } else if (yearly == T) {
      print(glue::glue("==> Fit final spline on individual years."))
      
      sym_year <- rlang::sym("YEAR") # quote for later merge
      char_year <- "YEAR"
      
      ## "Trick" spline with data elongation ----
      # before first year and after last year
      if (elong == T) {
        dat <- bind_rows(
          dat %>% filter(YEAR == min(YEAR)) %>% # duplicate data before first year
            mutate(YEAR = min(YEAR) - 1,
                   date = paste(YEAR, month(date), mday(date)) %>% as.Date(format = "%Y%m%d")
            ),
          dat, # actual data
          dat %>% filter(YEAR == max(YEAR)) %>% # duplicate data after last year
            mutate(YEAR = max(YEAR) + 1,
                   date = paste(YEAR, month(date), mday(date)) %>% as.Date(format = "%Y%m%d")
            ),
        )
      } # end 'elongation trick' if sentence
      ## Fit spline to single years ----
      YEARS <- dat %>% pull(YEAR) %>% unique() # year vector
      
      for (yyy in (2:(length(YEARS) - 1))) { # loop based on years to identify moving windows of 3 years
        year_window <- YEARS[(yyy-1):(yyy+1)] # moving window (3 years)
        dat_splin <- bind_rows(
          dat_splin,
          dat %>%
            filter(YEAR %in% year_window) %>% # filter moving window
            mutate(DOY = case_when(
              YEAR == year_window[1] ~ DOY - 366, # previous year
              YEAR == year_window[2] ~ DOY + 0,   # actual year
              YEAR == year_window[3] ~ DOY + 366  # next year
            )) %>% 
            group_by(cell) %>% 
            nest(s2_data = c(NDVI, date, YEAR, DOY, outlier)) %>%
            mutate(spline_out = map(.x = s2_data, .f = tidy_spline, x = "DOY", y = "NDVI",
                                    filt = "outlier", dfden = 10, sos = sos, eos = eos) # fit spline
            ) %>%
            select(-s2_data) %>% # remove s2 data! (comment to see full spline input)
            mutate(YEAR = year_window[2], .after = cell) %>%
            ungroup()
        )
      } # end '3-years moving window' loop
      # TO DO: add reference spline when datapoints are low?
      
      ## Remove empty output
      dat_splin <- dat_splin %>% # from spline output data
        drop_na(spline_out)
    } # end of 'if yearly' sentence
    
    
    ## Unnest and merge all data back to dataframe ----
    dat <- dat_splin %>% 
      unnest(cols = spline_out) %>%
      left_join(dat %>%
                  select(cell, DOY, NDVI, outlier, !!sym_year) %>%
                  rename(NDVIs2 = NDVI),
                by = c("cell", "DOY", char_year)
      )
    
    rm(dat_splin)
    gc()
    
    
    ## Set negative values to 0 ----
    dat <- dat %>% mutate(NDVI = if_else(NDVI < 0, 0, NDVI))
    
    
    ## Plot & save plot ----
    ## Settings
    if (transparent_plots == T) {
      theme_background <- theme( # transparent background
        panel.background  = element_rect(fill = 'transparent'), # transparent panel bg
        plot.background   = element_rect(fill = 'transparent', color = NA), # transparent plot bg
        # panel.grid.major = element_blank(), # remove major gridlines
        panel.grid.minor  = element_blank(), # remove minor gridlines
        legend.background = element_rect(fill = 'transparent'), # transparent legend bg
        legend.box.background = element_rect(colour = 'transparent', fill = 'transparent') # transparent legend panel and legend box
      )
      save_device <- "png"
      save_extension <- "png"
    } else if (transparent_plots == F) {
      theme_background <- NULL
      save_device <- "jpeg"
      save_extension <- "jpg"
    }
    
    ## Data for plotting
    if (yearly == F) {
      p_spline <- ggplot() +
        geom_point(data = dat %>% drop_na(NDVIs2), aes(x = DOY, NDVIs2, color = outlier), size = 1, shape = "o") +
        geom_line(data = dat %>% select(DOY, NDVI, cell) %>% unique(), aes(x = DOY, y = NDVI, group = cell), alpha = 0.15, size = 0.3) +
        scale_color_discrete(direction = -1) +
        ylab("NDVI") +
        theme_classic() +
        theme(
          axis.text = element_text(size = 16),
          axis.title = element_text(size = 20),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 16)
        ) +
        theme_background
      
      ## Save
      if (savedata == T) {
        ggsave(filename = glue::glue("results/phenodiv/final_spline_{site}_{save_string}.{save_extension}"),
               plot = p_spline, device = save_device,
               width = 500, height = 281.25, units = "mm", dpi = 300)
      }
    } else if (yearly == T) {
      YEARS <- dat %>% pull(YEAR) %>% unique() # year vector
      
      ## Plot
      for (yyy in (1:length(YEARS))) {
        dat_plot <- dat %>% 
          dplyr::filter(YEAR == YEARS[yyy])
        p_spline <- ggplot() +
          geom_point(data = dat_plot %>% drop_na(NDVIs2), aes(x = DOY, NDVIs2, color = outlier), size = 1, shape = "o") +
          geom_line(data = dat_plot %>% select(DOY, NDVI, cell) %>% unique(), aes(x = DOY, y = NDVI, group = cell), alpha = 0.15, size = 0.3) +
          scale_color_discrete(direction = -1) +
          labs(title = YEARS[yyy]) +
          ylab("NDVI") +
          theme_classic() +
          theme(
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 20),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 16)
          ) +
          theme_background
        
        ## Save
        if (savedata == T) {
          ggsave(filename = glue::glue("results/phenodiv/final_spline_{site}_{YEARS[yyy]}_{save_string}.{save_extension}"),
                 plot = p_spline, device = save_device,
                 width = 500, height = 281.25, units = "mm", dpi = 300)
        }
      }
    }
    
    
    
    ############################################################################
    ### Calculate phenological diversity metric ------------------------------------
    # method from Marco Girardello
    
    ## Step 1: cumulative sum of NDVI ----
    dat_phen <- dat %>%
      group_by(cell, !!sym_year) %>% # group by pixel
      mutate(ndvi_cumsum = cumsum(NDVI)) %>% # calculate cumulative sum of NDVI
      ungroup()
    
    # plot result
    if (savedata == T) {
      p1 <- dat_phen %>%
        ggplot() +
        geom_line(aes(x = DOY, y = ndvi_cumsum, group = cell),
                  colour = 'grey20', size = 0.1) +
        ylab("Cumulated NDVI") +
        theme_minimal() +
        theme(axis.text = element_text(size = 14, colour = "black"),
              axis.title = element_text(size = 17)
        ) +
        NULL
      
      ggsave(filename = glue::glue("results/phenodiv/step1_{site}_{save_string}.jpg"),
             plot = p1, device = "jpeg",
             width = 500, height = 281.25, units = "mm", dpi = 300)
    }
    
    
    ## Step 2: normalised cumulated NDVI ----
    dat_phen <- dat_phen %>%
      group_by(cell, !!sym_year) %>%
      mutate(ndvi_cumsum_norm = scales::rescale(ndvi_cumsum, to = c(0, 1))) %>% 
      ungroup()
    
    # plot result
    if (savedata == T) {
      p2 <- dat_phen %>%
        ggplot() +
        geom_line(aes(x = DOY, y = ndvi_cumsum_norm, group = cell),
                  colour = 'grey20', size = 0.1) +
        ylab("Cumulated NDVI") +
        theme_minimal() +
        theme(axis.text = element_text(size = 14, colour = "black"),
              axis.title = element_text(size = 17))
      
      ggsave(filename = glue::glue("results/phenodiv/step2_{site}_{save_string}.jpg"),
             plot = p2, device = "jpeg",
             width = 500, height = 281.25, units = "mm", dpi = 300)
    }
    
    
    ## Step 3: calculate deviations from the mean ----
    # for each time-step using the cumulated normalised NDVI
    dat_phen <- dat_phen %>%
      group_by(DOY, !!sym_year) %>%
      summarise(phe_std = sd(ndvi_cumsum_norm),
                phe_var = var(ndvi_cumsum_norm),
                ndvi_cumsum_norm_mean = mean(ndvi_cumsum_norm)) %>% 
      ungroup()
    
    # plot result
    if (savedata == T) {
      p3 <- p2 +
        geom_point(data = dat_phen, aes(x = DOY, y = ndvi_cumsum_norm_mean),
                   colour = myCol[3], size = 0.5) +
        geom_errorbar(data = dat_phen, aes(x = DOY, ymin = ndvi_cumsum_norm_mean - phe_std, ymax = ndvi_cumsum_norm_mean + phe_std),
                      colour = myCol[3], size = 0.5)
      
      ggsave(filename = glue::glue("results/phenodiv/step3_{site}_{save_string}.jpg"),
             plot = p3, device = "jpeg",
             width = 500, height = 281.25, units = "mm", dpi = 300)
    }
    
    
    ## Step 4: Extract metrics ----
    # i.e. calculate buffer average of all the deviations in
    # NDVI across the time period considered
    meandiv <- dat_phen %>%
      group_by(!!sym_year) %>% 
      mutate(phe_std_mean = mean(phe_std),
             phe_std_max = max(phe_std),
             phe_std_int = sum(diff(DOY[order(DOY)]) * zoo::rollmean(phe_std[order(DOY)],2))
      ) %>%
      ungroup() %>% 
      select(-ndvi_cumsum_norm_mean, -phe_std) %>%
      nest(phe_var = c(DOY, phe_var)) %>% 
      mutate(SITE_ID = site, .before = everything())
    # May need rescaling according to the offset in Sentinel 2 data. Need to ask Guido.
    
    
    
    ############################################################################
    ### Clear memory -----------------------------------------------------------
    rm(dat) # remove unnecessary data (important to avoid clogging memory during parallel computation(?))
    if (savedata) {rm(p1, p2, p3, p_spline)} # + p_series if un-commented
    gc() # clean memory usage
    
    
    
    ### Output -------------------------------------------------------------------
    return(meandiv)
    
    
    
    ### End of function ----------------------------------------------------------
  }, error = function(err) {
    warning(paste0("Unforeseen error in the calculation of phenodiv, skipping site ", site, "."))
    return(err_output)
  }
  ) # end tryCatch
} # end function

#### Debug ---------------------------------------------------------------------
# debugonce(calc_phenodiv)