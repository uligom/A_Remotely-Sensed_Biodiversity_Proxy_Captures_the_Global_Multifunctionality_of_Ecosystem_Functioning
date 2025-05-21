do_crossval_relaimpo <- function(
    data = dat_norm, efps_names = efps_names, predictors = predictors_bb,
    savedata = savedata, eval_file = eval_file, vers_out = vers_out
    # unsorted_predictors = F, # for different input format of object of predictors
) {
  ### Utilities ----------------------------------------------------------------
  require(dplyr)
  require(MuMIn)
  require(rlang)
  require(tidyr)
  
  # source("scripts/functions/dredged_relaimpo.R")
  
  
  ### Loop EFPs ----------------------------------------------------------------
  relimp_all <- tibble() # initialize tibble of relative importance results
  
  for (ii in 1:length(efps_names)) {
    # 1) Subset data ----
    EFP_x <- rlang::sym(efps_names[[ii]])
    
    txt <- glue::glue("==> Performing multimodel inference to explain {EFP_x}.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    
    if (class(predictors)[1] == "tbl_df") {
      # Handle missing main predictors for certain EFP (main analysis)
      if (!efps_names[[ii]] %in% unique(predictors$prediction)) {
        txt <- glue::glue("Model could not be performed for {EFP_x}: no input predictors.")
        print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
        next
      }
      
      # Extract predictors
      predictors_efpx <- predictors %>% dplyr::filter(prediction == as.character(EFP_x)) %>% dplyr::pull(variable)
      
    } else {
      # Extract predictors
      predictors_efpx <- predictors
    }

    # Extract variables for the model (select e.g. GPPSAT and predictors)
    df <- data %>%
      dplyr::select(SITE_ID, !!!predictors_efpx, !!EFP_x) %>% # here it is important to only select one predicted variable, and all previosuly selected predictors (through)
      tidyr::drop_na()
    
    ## Save input data ----
    if (savedata) {
      efp_vers_out <- paste0(vers_out, "_", as.character(EFP_x))
      # Site list
      dat_sites <- df %>% dplyr::select(SITE_ID)
      write_csv(dat_sites, glue::glue("data/output/site_list_cross_validation_{efp_vers_out}.csv"))
    }
    df <- df %>% dplyr::select(-SITE_ID) # remove site index for numerical analysis
    
    # 2) Linear Model ----
    ## Remove correlated predictors after VIF
    formula0 <- as.formula(glue::glue("{EFP_x} ~ ."))
    fm0 <- lm(formula0, data = df)
    
    # 2.1) Summary
    summar <- summary(fm0) # summarize results
    
    
    # 3) Cross validation ----
    rmse_efpx <- loo(fm0, type = "rmse")
    
    
    # 4) Calculate importance of predictors ----
    if (length(predictors_efpx) > 1) {
      relimp <- relaimpo::calc.relimp(fm0, rela = T)
      
      # Compile output tibble
      relimp_all <- bind_rows(relimp_all,
                              tibble(
                                prediction = as.character(EFP_x),
                                variable = names(relimp@lmg),
                                R2 = summar$r.squared,
                                R2_adjusted = summar$adj.r.squared,
                                AICc = AICc(fm0), # k = 2, should be equal to default AIC, but it's not
                                rel_importance = unname(relimp@lmg),
                                n = nrow(df),
                                RMSE = rmse_efpx # leave-one-out root mean square error for cross validation
                              ) %>%
                                left_join(summar$coefficients %>%
                                            as_tibble(rownames = "variable") %>%
                                            dplyr::filter(variable != "(Intercept)") %>%
                                            dplyr::rename(p_val = `Pr(>|t|)`) %>%
                                            janitor::clean_names(),
                                          by = "variable")
      )
    } else if (length(predictors_efpx) == 1) {
      # Compile output tibble
      relimp_all <- bind_rows(relimp_all,
                              bind_cols(
                                prediction = as.character(EFP_x),
                                variable = rownames(summar$coefficients)[2],
                                R2 = summar$r.squared,
                                R2_adjusted = summar$adj.r.squared,
                                AICc = AICc(fm0), # k = 2, should be equal to default AIC, but it's not
                                rel_importance = 1,
                                n = nrow(df),
                                RMSE = rmse_efpx, # leave-one-out root mean square error for cross validation
                              ) %>%
                                left_join(summar$coefficients %>%
                                            as_tibble(rownames = "variable") %>%
                                            dplyr::filter(variable != "(Intercept)") %>%
                                            dplyr::rename(p_val = `Pr(>|t|)`) %>%
                                            janitor::clean_names(),
                                          by = "variable")
      )
    } else {
      warning("Relative importance could not be performed because input object had 0 predictors.")
    } # end if 'number of predictors > 1'
    
  } # end for loop for cross validation + relative importance analysis on single EFPs
  
  
  ### Output -------------------------------------------------------------------
  return(relimp_all)
} # end function


### Debug ----------------------------------------------------------------------
# debugonce(do_crossval_relaimpo)