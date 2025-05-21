#### FUNCTION TO ITERATIVELY PERFORM RELATIVE IMPORTANCE ON A SET OF MODELS
# Typical input is a model inference output (function MuMIn::dredge)
# or a subset thereof.

### Version history ------------------------------------------------------------
# changed number extraction from strings to function strex::str_extract_number

### Arguments ------------------------------------------------------------------
# data      Input tibble with all needed predictor and predicted variables.
# y         Predicted variable.
# models    Tibble with information on coefficients and model weights in the
#           same form as the 'dredge' function output.


### Function -------------------------------------------------------------------
dredged_relaimpo <- function(data, y = "GPPsat", models) {
  ## Utilities ----
  require(dplyr)
  require(relaimpo)
  require(strex)
  require(stringr)
  require(tidyr)
  
  
  ## Inputs ----
  if (nrow(models) > 1) {
    ## Summary of dredge object ----
    summar <- summary(model.avg(object = models, revised.var = FALSE))
    
    ## Extract variable names from models ----
    # Extract term coding
    term_translation <- dplyr::tibble(
      term = names(attributes(summar$msTable)$term.codes),
      term.code = unname(attributes(summar$msTable)$term.codes)
    )
    
    # Term codes in models
    model_terms <- attributes(summar$msTable)$row.names
  }
  
  for (i in 1:nrow(models)) {
    if (models$df[i] > 2) { # exclude model with only intercept
      ## Formulas for glm ----
      if (nrow(models) > 1) {
        term.code <- strex::str_extract_numbers(model_terms[i]) %>% unlist() # extract codes for variables used
        if (length(term.code) == 1) { # extract codes for variables used, again with different function since 'model.avg' gives different outputs
          term.code <- str_extract_all(model_terms[i], "[:digit:]+") %>% unlist() %>% as.integer()
          
          if (!term.code %in% term_translation$term.code) { # correct codes when incorrectly extracted because of different model.avg output (e.g. 13 instead of 1 and 3)
            term.code <- str_extract_all(model_terms[i], "[:digit:]{1}") %>% unlist() %>% as.integer()
          }
        }
        
        single_terms <- dplyr::tibble(term.code = term.code) %>% # first column with codes
          dplyr::left_join(term_translation, by = "term.code") # second column with symbols
        
        model_strings <- paste0(y, " ~ ", paste(single_terms$term, collapse = " + ")) %>% 
          as.formula() # formulas for glm
        
      } else if (nrow(models) == 1) {
        model_strings <- formula(attributes(models)$model.calls[[1]])
        single_terms <- as.character(model_strings)[3]
        single_terms <- stringr::str_extract_all(single_terms, pattern = "[:graph:]+(?=([:blank:]{1}[:graph:]{1}[:blank:]{1}))") # extract predictors from formula
        single_terms <- single_terms %>% unlist() %>% as_tibble() %>% dplyr::rename(term = value) # convert to tibble for compatibility to following code
      }
      
      ## LM ----
      fm <- lm(model_strings, data = data)
      
      if (nrow(single_terms) != 1) {
        ## Relative importance ----
        relimp <- relaimpo::calc.relimp(fm, rela = T)
        
        
        ## extract lmg, R2, AICc, and RMSE as tibble
        # with one entry per variable
        lmg_i <- dplyr::tibble(term = names(relimp@lmg),                  # extract variable names
                               lmg = unname(relimp@lmg),                  # extract lmg values for variables
                               weight = models$weight[i] %>% as.double()) # model weight
        # with single entries per EFP
        eval_i <- dplyr::tibble(R2 = relimp@R2,                           # extract R2 (same as lm 'Multiple R-squared'!)
                                AICc = models$AICc[i],                    # extract AICc
                                RMSE = sqrt(mean(fm$residuals^2)),        # extract RMSE
                                weight = models$weight[i] %>% as.double() # model weight
        )
      } else if (nrow(single_terms) == 1) {
        lmg_i <- tibble(term = single_terms$term, lmg = 1, weight = models$weight[i] %>% as.double()) # extract lmg values for variables
        eval_i <- dplyr::tibble(R2 = summary(fm)$r.squared,               # extract R2
                                AICc = models$AICc[i],                    # extract AICc
                                RMSE = sqrt(mean(fm$residuals^2)),        # extract RMSE
                                weight = models$weight[i] %>% as.double() # model weight
        )
      }
    } else if (models$df[i] == 2) {
      lmg_i <- tibble(term = NA_character_, lmg = NA_real_, weight = NA_real_) # extract lmg values for variables
      eval_i <- dplyr::tibble(R2 = NA_real_, AICc = NA_real_, RMSE = NA_real_, weight = NA_real_)
    }
    
    # bind ith lmg, R2 and AICc data to tibble with all models in long format
    if (i == 1) {
      lmg_all <- lmg_i
      eval_all <- eval_i
    } else {
      lmg_all <- lmg_all %>% rbind(lmg_i)
      eval_all <- eval_all %>% rbind(eval_i)
    }
  }
  
  ## Weighted mean of relative importance (lmg) ----
  # based on weights from dredge output
  lmg_all <- lmg_all %>% 
    dplyr::filter(!is.na(term)) %>% 
    dplyr::mutate(wlmg = lmg * weight) %>%  # calculate products
    dplyr::group_by(term) %>% 
    dplyr::summarise(w_lmg = sum(wlmg)) %>% # calculate weighted mean (i.e. summation of products over all models, because multiplied by weights, which sum up to 1)
    dplyr::ungroup() %>% 
    dplyr::arrange(dplyr::desc(w_lmg)) %>% 
    tidyr::pivot_wider(names_from = term, values_from = w_lmg)
  
  ## Weighted mean of evaluation metrics (R2, AICc, RMSE) ----
  eval_all <- eval_all %>% 
    dplyr::filter(rowSums(.) != 3) %>% 
    dplyr::mutate( # calculate products
      wR2 = R2 * weight,
      wAICc = AICc * weight,
      wRMSE = RMSE * weight
      ) %>%
    dplyr::summarise( # calculate weighted means (sum, because previously multiplied by the weights)
      R2 = sum(unique(wR2)),
      AICc = sum(unique(wAICc)),
      RMSE = sum(unique(wRMSE))
      )
  
    
  ## Output single dataframe with weighted lmg and R2 ----
  weighted_relimp <- dplyr::bind_cols(lmg_all, eval_all)
  
  return(weighted_relimp)
}
### Debug ----------------------------------------------------------------------
# debugonce(dredged_relaimpo)