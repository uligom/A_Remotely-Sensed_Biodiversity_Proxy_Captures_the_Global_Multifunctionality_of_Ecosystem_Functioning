#### Function ----------------------------------------------------------------
# Test relationships between ecosystem functions/multifunctionality and biodiversity

### Authors: Ulisse Gomarasca (ugomar@bgc-jena.mpg.de)

run_multimodel_inference <- function(test = "main") {
  ### Version History ----------------------------------------------------------
  # v00, 06.03.2024:  Same as v11 of BEF analysis.
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
  # v01, 22.03.2024:  Added options for analysis on EFP stability and multifunctionality.
  #      08.05.2024:  Removed soil variables.
  # v01b,14.05.2024:  Testing measured LAImax in models.
  # v02, 21.05.2024:  Testing NIRv_median together with RaoQ_NIRv.
  # v03, 27.05.2024:  Testing soil variables.
  # v04, 27.05.2024:  Testing only IAV climate variables.
  
  
  
  ### Function input -----------------------------------------------------------
  if (test == "main") {test_vers <- "v01."}
  else if (test == "lai") {test_vers <- "v01b."}
  else if (test == "nirv") {test_vers <- "v02."}
  else if (test == "soil") {test_vers <- "v03."}
  # else if (test == "iav") {test_vers <- "v04."}
  
  
  
  ### Script settings ----------------------------------------------------------
  library(tictoc)
  tic("Multimodel Inference script: ") # measure run time
  
  ## Data settings
  savedata <- as.logical(readline(prompt = "Save the output of the script? T/F:")) # ask if output should be saved
  efp_in <- as.character(read.table("data/efp_version.txt"))
  emf_in <- as.character(read.table("data/emf_version.txt"))
  # iav_in <- as.character(read.table("data/iav_version.txt"))
  dat_in <- as.character(read.table("data/data_version.txt"))
  vers <- paste0(test_vers, stringr::str_extract(dat_in, "[:digit:]+[:punct:]?[:digit:]*[:punct:]?[:digit:]*"))
  cat(paste0(vers, "\n"), file = "data/mumin_analysis_version.txt") # save version number for reference in other scripts ("\n" for new line to avoid warning when reading back into R with read.table())
  
  
  
  ### Utilities ------------------------------------------------------------------
  ## Packages
  library(car)          # variable inflation factor (vif)
  library(dplyr)        # tidy data manipulation
  options(dplyr.summarise.inform = F) # suppress summary info
  library(ggplot2)      # tidy plots
  library(modEvA)       # pseudo-R2 for glm
  library(MuMIn)        # dredge multimodel inference
  library(RColorBrewer) # plot color functionalities
  library(readr)        # read table format files
  library(relaimpo)     # relative importance analysis
  library(rlang)        # quoting
  library(rstatix)      # levene's normality test
  library(stringr)      # string manipulation
  library(tictoc)       # measure time
  library(tidyr)        # clean and reshape tidy data
  library(tidyselect)   # tidyverse helpers
  
  ## Functions
  source("scripts/functions/do_crossval_relaimpo.R")
  source("scripts/functions/do_multimodel_relaimpo.R")
  source("scripts/functions/min_max_norm.R")
  # source("scripts/functions/plot_scatterplot_models.R")
  
  ## Other
  source("scripts/themes/MyThemes.R")
  
  
  
  ### Analysis -----------------------------------------------------------------
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
    vers_out <- glue::glue("{response}_{subx}_{raoq_in}_{vers}")
    
    vers_all <- glue::glue("{response}_all_{raoq_in}_{vers}") # last version for which analysis with all variables was performed (should be same as vers_out when fully updated)
    
    eval_file <- glue::glue("results/analysis_evaluation/evaluation_multivariate_analysis_{vers_out}.txt")
    txt <- "Initializing analysis..."; print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = F)}
    
    
    
    ### Data ---------------------------------------------------------------------
    if (response == "efp") { # analysis on full-timeseries EFPs
      dat <- read_csv(glue::glue("data/inter/data4analysis_efps_{dat_in}.csv"), show_col_types = F) %>% glimpse()
      # } else if (response == "iav") { # analysis on stability of EFPs
      #   dat <- read_csv(glue::glue("data/inter/data4analysis_stability_{dat_in}.csv"), show_col_types = F) %>% glimpse()
    } else if (response == "emf") { # analysis on multifunctionality of EFPs
      dat <- read_csv(glue::glue("data/inter/data4analysis_multifunctionality_{dat_in}.csv"), show_col_types = F) %>% glimpse()
    }
    
    
    
    ### Vector names -------------------------------------------------------------
    ## Response variable(s)
    if (response == "efp") { # analysis on full-timeseries EFPs
      load(file = glue::glue("data/inter/efps_names_{efp_in}.RData"))
      # } else if (response == "iav") { # analysis on stability of EFPs
      #   load(file = glue::glue("data/inter/efps_iav_names_{iav_in}.RData"))
    } else if (response == "emf") { # analysis on multifunctionality of EFPs
      load(file = glue::glue("data/inter/emf_names_{emf_in}.RData"))
    }
    
    ## Climate
    if (response == "efp") { # analysis on full-timeseries EFPs
      load(file = glue::glue("data/inter/clim_names_{efp_in}.RData"))
      # } else if (response == "iav") { # analysis on stability of EFPs
      #   load(file = glue::glue("data/inter/clim_names_{efp_in}.RData")) # mean climate
      #   load(file = glue::glue("data/clim_iav_names_{iav_in}.RData")) # climate variability
    } else if (response == "emf") { # analysis on multifunctionality of EFPs
      load(file = glue::glue("data/inter/clim_names_{efp_in}.RData"))
    }
    
    ## Structure
    load(file = glue::glue("data/inter/struct_names_{efp_in}.RData"))
    
    
    
    ### Process data -----------------------------------------------------------
    ## Pre-selection of EFPs and predictors ----
    if (raoq_in == "bands") {raoQ_name <- sym("Rao_Q_S2")
    } else if (raoq_in == "ndvi") {raoQ_name <- sym("Rao_Q_NDVI")
    } else if (raoq_in == "nirv") {raoQ_name <- sym("Rao_Q_NIRv")
    }
    
    dat <- dat %>% 
      dplyr::select(-IGBP, -contains("LONGITUDE"), -contains("LATITUDE"), -contains("SOURCE")) %>% # exclude metadata
      dplyr::select(-contains("99"), -contains("pval")) %>% # exclude unnecessary metrics (e.g. NEP99, CUEpval90)
      dplyr::select(-contains("Rao_Q"), !!raoQ_name) # satellite Rao's Q: only keep one
    
    ## Test-based selections
    if (test == "lai") {
      dat <- dat %>%
        dplyr::select(-LAImax_modis) # exclude LAImax_modis for testing differences (v01b)
    } else if (test != "lai") {
      dat <- dat %>%
        dplyr::select(-LAImax) %>% rename(LAImax = LAImax_modis) # replace LAImax with MODIS LAImax (since LAImax_modis is more thourough) (every version other than v01b)
    }
    
    if (test == "nirv") {
      dat <- dat %>%
        left_join(
          read_csv("data/inter/raoq/raoQ_S2_L2A_filtered_averaged.csv", show_col_types = F) %>% # add NIRv (v02)
            select(SITE_ID, NIRv_median),
          by = "SITE_ID"
        )
    }
    
    if (test != "soil") {
      dat <- dat %>%
        dplyr::select(-contains(c("CLAY", "SAND", "SILT", "AWCh", "ORCDRC", "PHIHOX"))) # exclude soil predictors (from modelled grid product) (all versions other than v03)
    }
    
    # if (test == "iav") {
    #   dat <- dat %>%
    #     dplyr::select(-Temp, -Precip, -SWin, -VPD) # test without mean climatic variables (v04)
    # }
    
    # dplyr::select(SITE_ID, contains("EMF"), !!raoQ_name, LAImax, Hc) # for EMFsubset test with only significant predictors
    
    
    ## Select vector names ----
    ## Response variable(s)
    if (response == "efp") { # analysis on full-timeseries EFPs
      y_names <- efps_names[efps_names %in% names(dat)]
    } else if (response == "iav") { # analysis on stability of EFPs
      y_names <- efps_iav_names[efps_iav_names %in% names(dat)]
    } else if (response == "emf") { # analysis on multifunctionality of EFPs
      y_names <- emf_names[emf_names %in% names(dat)]
    }
    
    ## Climate
    if (response == "efp") { # analysis on full-timeseries EFPs
      clim_names <- clim_names[clim_names %in% names(dat)]
    } else if (response == "iav") { # analysis on stability of EFPs
      clim_names <- c(clim_names[clim_names %in% names(dat)], clim_iav_names[clim_iav_names %in% names(dat)])
    } else if (response == "emf") { # analysis on multifunctionality of EFPs
      clim_names <- clim_names[clim_names %in% names(dat)]
    }
    
    ## Structure
    struct_names <- struct_names[struct_names %in% names(dat)]
    
    
    
    ### Multivariate analysis --------------------------------------------------
    ## Normalize data between 0 and 1 ----
    dat_norm <- dat %>% mutate(across(.cols = where(is.double), .fns = min_max_norm))
    
    
    ## Select predictors ----
    # Extract variables for the model (select e.g. GPPSAT (for vif testing) and predictors)
    y_test <- sym(dat_norm %>% select(contains("GPPsat") | contains("EMFavg")) %>% names())
    df_vifed <- dat_norm %>% 
      dplyr::select(!c(SITE_ID, !!!syms(y_names)), !!y_test) %>% # here it doesn't matter which variable is being predicted, we are just testing the predictors
      tidyr::drop_na()
    
    
    ## Test collinearity (VIF) ----
    options(na.action = "na.fail") # global options for na.action (needed)
    
    txt <- "==> Computing Variance Inflation Factor to select explanatory variables for further analyses."
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    
    df_subvifed <- df_vifed # final VIF on all (but EFPs) variables: align dataframes for VIF processing
    n_excl <- 1 # initialize
    var_excl_list <- c()
    while (n_excl > 0) {
      ## Generalized Linear Model with all predictors
      # (cf. normality assumption)
      formula1 <- as.formula(glue::glue("{y_test} ~ .")) # here it doesn't matter which variable is being predicted, we are just testing the predictors
      fm1 <- glm(formula1, data = df_subvifed) # generalized linear model
      
      ## calculate variable inflation factor
      vif_excl <- bind_rows(vif(fm1)) %>%
        pivot_longer(everything(), names_to = "variable", values_to = "inflation_factor") %>% 
        print(n = Inf)
      
      # check how many variables are have vif above threshold
      n_excl <- vif_excl %>% 
        dplyr::filter(inflation_factor > 10) %>%
        nrow()
      
      txt <- glue::glue("{n_excl} variables above inflation factor of 10.")
      print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
      
      if (n_excl > 0) {
        ## exclude variables with values over 10
        var_excl <- vif_excl %>%
          dplyr::filter(inflation_factor > 10) %>% 
          dplyr::filter(inflation_factor == max(inflation_factor)) %>% 
          pull(variable) %>% 
          rlang::sym()
        
        var_excl_list <- c(var_excl_list, as.character(var_excl))
        
        txt <- glue::glue("Removing variable with highest vif value ({var_excl}).")
        print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
        
        df_vifed <- df_vifed %>% dplyr::select(-!!var_excl) # remove variable from list of explanatories
        df_subvifed <- df_subvifed %>% dplyr::select(-!!var_excl) # remove variable from subset
      }
    }
    
    txt <- glue::glue("Test of variance inflation factor excluded the following variables: {paste(var_excl_list, collapse = ', ')}.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    
    predictors <- df_vifed %>% select(-!!y_test) %>% names() %>% print()
    
    
    # ## Check assumptions for regression ----
    # txt <- "==> Testing assumption of normality on variables' residuals."
    # print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    # 
    # ## Normal distribution of predictors:
    # # perform Shapiro-Wilk's test
    # shapiro_stats <- dat_norm %>%
    #   dplyr::select(all_of(predictors)) %>% 
    #   rstatix::shapiro_test(vars = names(dat_norm %>% dplyr::select(all_of(predictors) & where(is.numeric)))) %>%
    #   mutate(normality = if_else(p > 0.1, TRUE, FALSE))
    # shapiro_stats
    # 
    # txt <- glue::glue("{nrow(shapiro_stats) - shapiro_stats %>% pull(normality) %>% sum()} out of {nrow(shapiro_stats)} variables did NOT meet the assumption of normality.")
    # print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    # 
    # # # Transform data?
    # # rcompanion::transformTukey()
    # 
    # # Residual plot
    # plot(fm1)
    # 
    # 
    
    ### Multimodel inference - one variable per group at a time ----------------
    if (subx == "no") { # NO BIODIVERSITY ----
      # Approximate running time: 30 seconds
      ## Select biodiversity predictors
      predictors_bb <- predictors[predictors %in% c(clim_names, struct_names)]
      
      ## Announce analysis
      txt <- glue::glue("==> Analysis on {subx} biodiversity predictors.")
      print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
      
      ## Do analysis via function
      relimp_all <- do_multimodel_relaimpo(dat_norm, y_names, predictors_bb, savedata, eval_file, vers_out)
      
      ## Save
      if (savedata) {
        # model output and variable importance
        write_csv(relimp_all, glue::glue("results/multimodel_inference/prediction_and_relaimpo_{vers_out}.csv"))
      }
      
      
      
    } else if (subx == "all") { # ALL BIODIVERSITY ----
      # Approximate running time: > 1 hour
      ## Select biodiversity predictors
      predictors_bb <- predictors
      
      ## Announce analysis
      txt <- glue::glue("==> Analysis on {subx} biodiversity predictors.")
      print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
      
      ## Do analysis via function
      relimp_all <- do_multimodel_relaimpo(dat_norm, y_names, predictors_bb, savedata, eval_file, vers_out)
      relimp_crossval <- do_crossval_relaimpo(dat_norm, y_names, predictors_bb, savedata, eval_file, vers_out)
      
      ## Attach loo RMSE
      relimp_all <- relimp_all %>% 
        left_join(relimp_crossval %>% dplyr::select(prediction, RMSE) %>% unique(), by = "prediction", suffix = c("", "_loo")) %>% 
        relocate(RMSE_loo, .after = RMSE)
      
      ## Save ----
      if (savedata) {
        # model output and variable importance
        write_csv(relimp_all, glue::glue("results/multimodel_inference/prediction_and_relaimpo_{vers_out}.csv"))
      }
      
      
      ## Calculate statistics for manuscript ----
      if (eee == 1) {
        ## Average relative importance of Rao Q for all EFPs
        avg_relaimpo_RaoQ <- relimp_all %>% dplyr::filter(variable == "Rao_Q_NIRv") %>% pull(rel_importance) %>% mean(na.rm = T)
        avg_relaimpo_RaoQ
        ## Save
        if (savedata) {
          cat(
            glue::glue("the average relative importance of Rao Q for all six EFPs was {round(avg_relaimpo_RaoQ * 100, digit = 1)} %"),
            file = glue::glue("results/multimodel_inference/avg_relaimpo_RaoQ_{vers_out}.txt")
          )
        }
      }
      
      
    } else {
      warning("Contrasting conditions. Subset biodiversity predictors AND subdivide into ground and satellite?")
    } # end conditions
    
    
  } # end loop for type of predicted variables (EFPs vs EMFs)
  
  
  
  ### End ----------------------------------------------------------------------
  toc()
}



# #### Debug ---------------------------------------------------------------------
# debugonce(run_multimodel_inference)