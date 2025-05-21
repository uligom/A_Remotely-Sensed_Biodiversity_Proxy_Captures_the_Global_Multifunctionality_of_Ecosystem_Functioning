#### Tidy function to do spline smoothing

### Arguments ------------------------------------------------------------------
# x       indipendent variable (character)
# y       dependent variable (character)
# filt    filter variable, as T/F (character). Set as "none" (default) for no filtering
# dfden   adjusts spline sensibility (numeric)
# sos     start of fitting period, e.g. start of season (numeric)
# eos     end of fitting period, e.g. end of season (numeric)



#### Function ------------------------------------------------------------------
tidy_spline <- function(data = ., x = "DOY", y = "NDVI", filt = "none", dfden = 12, sos = 1, eos = 366) {
  ### Utilities ----
  require(dplyr)
  require(rlang)
  require(tidyr)
  
  
  
  ### Quote ----
  x_sym <- rlang::sym(x)
  y_sym <- rlang::sym(y)
  filt_sym <- rlang::sym(filt)
  
  
  
  ### Filter ----
  if (filt != "none") {
    data <- data %>% 
      dplyr::filter(!!filt_sym == F) # filter only when FALSE
  }
  
  
  
  ### Drop NA ----
  data <- data %>% 
    dplyr::select(!!x_sym, !!y_sym) %>% 
    tidyr::drop_na()
  
  
  if (nrow(data) > 4) { # do spline where enough data entries are available
    ### Variables ----
    X <- data %>% dplyr::select(!!x_sym) %>% unlist() %>% unname()
    Y <- data %>% dplyr::select(!!y_sym) %>% unlist() %>% unname()
    
    
    
    ### Spline ----
    splin_list <- stats::smooth.spline(x = X, y = Y, df = length(Y) / dfden)
    splin_vecs <- stats::predict(splin_list, x = sos:eos)
    
    
    
    ### Convert ----
    splin_tbl <- as_tibble(splin_vecs) %>% dplyr::rename("{x_sym}" := x, "{y_sym}" := y)
    
    
    
    ### Output ----
    return(splin_tbl)
    
    
    
  } else { # otherwise do not apply spline and return empty vector
    suppressMessages(message("Spline could not be applied: not enough data provided."))
    
    
    ### Output ----
    return(NA)
  }
  
  
}

#### Debug ---------------------------------------------------------------------
# debugonce(tidy_spline)
# tidy_spline(data = dat_splin)