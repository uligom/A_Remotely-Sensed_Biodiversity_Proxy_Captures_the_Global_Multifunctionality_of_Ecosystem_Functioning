### Function -------------------------------------------------------------------
do_multimodel_relaimpo <- function(
    data = dat_norm, efps_names = efps_names, predictors = predictors_bb,
    savedata = savedata, eval_file = eval_file, vers_out = vers_out
) {
  ### Utilities ----------------------------------------------------------------
  require(dplyr)
  require(MuMIn)
  require(rlang)
  require(tictoc)
  require(tidyr)
  
  source("scripts/functions/dredged_relaimpo.R")
  
  
  ### Loop EFPs ----------------------------------------------------------------
  relimp_all <- tibble() # initialize tibble of relative importance results
  
  for (ii in 1:length(efps_names)) {
    # 1) Subset data ----
    EFP_x <- rlang::sym(efps_names[[ii]])
    
    txt <- glue::glue("==> Performing multimodel inference to explain {EFP_x}.")
    print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    
    
    ## Extract predictors for specific EFP
    if (class(predictors)[1] == "tbl_df") {
      # Handle missing main predictors for certain EFP (main analysis)
      if (!efps_names[[ii]] %in% unique(predictors$prediction)) {
        txt <- glue::glue("Model could not be performed for {EFP_x}: no input predictors.")
        print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
        next
      }
      
      # Extract predictors
      predictors_efp <- predictors %>% dplyr::filter(prediction == as.character(EFP_x)) %>% dplyr::pull(variable)
    
    } else {
      # Extract predictors
      predictors_efp <- predictors
    }
    
    
    # Extract variables for the model (select e.g. GPPSAT and predictors)
    df <- data %>%
      dplyr::select(SITE_ID, !!!predictors_efp, !!EFP_x) %>% # here it is important to only select one predicted variable, and all previosuly selected predictors (through)
      tidyr::drop_na() # important to drop NA (not accepted by dredge), since this step was removed from the whole dataframe processing
    
    
    ## Save input data ----
    if (savedata) {
      efp_vers_out <- paste0(vers_out, "_", as.character(EFP_x))
      # Data
      write_csv(df, glue::glue("data/output/data_mumin_input_{efp_vers_out}.csv"))
      # Site list
      dat_sites <- df %>% dplyr::select(SITE_ID)
      write_csv(dat_sites, glue::glue("data/output/site_list_{efp_vers_out}.csv"))
    }
    df <- df %>% dplyr::select(-SITE_ID) # remove site index for numerical analysis
    
    
    # 2) Model selection ----
    formula0 <- as.formula(glue::glue("{EFP_x} ~ ."))
    fm0 <- lm(formula0, data = df)
    
    
    # 2.1) feed output(s) to dredge separately and confront AIC
    # NB: output size (and computing time!) of 'dredge' function increases exponentially with the number of predictors
    # look into 'pdredge' for a version with parallel computing
    # if (gb == 1 & sr == 1) {
    #   dd0 <- dredge(fm0, beta = "partial.sd", rank = AICc) # initialize output of loops
    # } else {
    # tic() # start timer
    
    # Automated model selection
    dd0 <- dredge(fm0, beta = "partial.sd", rank = AICc)
    
    # time_dredge <- toc() # stop timer
    # time_dredge <- round(time_dredge$toc - time_dredge$tic, digits = 2) # format
    # if (time_dredge < 60) {
    #   time_qual <- "seconds"
    # } else if (time_dredge > 60 & time_dredge < 3600) {
    #   time_qual <- "minutes"; time_dredge <- round(time_dredge / 60, digits = 2)
    # } else if (time_dredge > 3600) {
    #   time_qual <- "hours"; time_dredge <- round(time_dredge / 3600, digits = 2)
    # }
    # 
    # 
    # if (ii == 1) {
    #   txt <- glue::glue("Run time of multi-model inference for one EFP ({EFP_x}) under this setup was {time_dredge} {time_qual}.
    #                     Remaining expected run time is around {time_dredge * length(efps_names) - time_dredge} {time_qual}.")
    #   print(txt)
    #   if (!out_bb %in% c("main", "no_main", "no")) {continue_tf <- as.logical(readline(prompt = "Proceed? T/F:")) # ask if analysis should be continued
    #   } else {continue_tf <- T} # automatically continue for 'main' analysis (since they are quick)
    # }
    # if (continue_tf == F) { # stop loop after analysis of first EFP
    #   txt <- glue::glue("NB: Analysis of further EFPs was skipped. Only results for {EFP_x} will be outputted!")
    #   print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    #   break
    # }
    
    
    # 2.2) Extract ensemble of best models
    max_delta <- 4
    dlt40 <- subset(dd0, delta < max_delta) # Extract models with difference in AIC from best model's AIC < 2 or 4. Generally < 4 is suggested unless more restrictive
    
    
    # 2.3) Summary
    summar <- summary(model.avg(object = dd0, revised.var = FALSE)) # summarize results
    # The 'subset' (or 'conditional') average only averages over the models where the parameter appears.
    # An alternative, the 'full' average assumes that a variable is included in every model,
    # but in some models the corresponding coefficient (and its respective variance) is set to zero.
    # Unlike the 'subset average', the full average does not have a tendency of biasing the value away from zero.
    # The 'full' average is a type of shrinkage estimator, and for variables with a weak relationship
    # to the response it is smaller than 'subset' estimators.
    
    
    # 3) Calculate importance of predictors ----
    ## Relative importance over all models (lmg) and weighted mean of lmg
    df <- data %>% # redefine model input data
      dplyr::select(!!!predictors_efp, !!EFP_x) %>% # here it is important to only select one predicted variable, and all previosuly selected predictors (through)
      tidyr::drop_na()
    
    ## Normal Analysis on AICc
    relimp <- tryCatch( # try to perform relative importance analysis, otherwise output warning and empty file
      {dredged_relaimpo(data = df, y = as.character(EFP_x), models = dlt40) %>% # variable values are weighted lmg; R2 is weighted over all models
          tidyr::pivot_longer(cols = !c(R2, AICc, RMSE), names_to = "variable", values_to = "rel_importance") %>%  # transform in long format with column for lmg
          dplyr::mutate(n = nrow(df)) %>%
          dplyr::relocate(variable, .before = R2)
        
      }, error = function(err) {
        txt <- glue::glue("Relative importance could not be correctly performed for {EFP_x}. Skipping current model.")
        warning(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
        return(tibble(variable = NA_character_,	R2 = NA_real_, AICc = NA_real_, RMSE = NA_real_, rel_importance = NA_real_, n = NA_real_))
        next
      })
    
    if (relimp[1,] %>% is.na() %>% sum() != length(relimp)) {
      txt <- glue::glue("Multimodel inference and relative importance based on Akaike's Information Criterion corrected for small sample sizes correctly performed for {EFP_x}.")
      print(txt); if (savedata) {cat(paste0(txt, "\n"), file = eval_file, append = T)}
    }
    
    
    # 4) Prepare Output ----
    relimp_all <- bind_rows(relimp_all,
                            bind_cols(
                              prediction = as.character(EFP_x),
                              relimp
                            ) %>% 
                              left_join(summar$coefmat.subset %>% # full averages: variables are assumed to be included in every model, and effects set to zero if missing.
                                          as_tibble(rownames = "variable") %>%
                                          dplyr::filter(variable != "(Intercept)") %>% 
                                          dplyr::rename(p_val = `Pr(>|z|)`) %>% # p-value of multimodel inference (p-value of relaimpo missing from output!)
                                          janitor::clean_names(),
                                        by = "variable")
    )
    
  } # end for loop for multimodel inference + relative importance analysis on single EFPs
  
  
  ### Output -------------------------------------------------------------------
  return(relimp_all)
  
  
  
} # end function


### Debug ----------------------------------------------------------------------
# debugonce(do_multimodel_relaimpo)