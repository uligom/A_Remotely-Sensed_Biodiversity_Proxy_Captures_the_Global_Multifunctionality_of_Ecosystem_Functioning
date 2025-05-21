### Function -------------------------------------------------------------------
import_COORDS <- function(
    site_list = list("AR-SLu", "AT-Neu", "AU-Cpr"),
    path = "//minerva/BGI/scratch/jnelson/4Sinikka/data20240123/",
    future_env = list(savedata, eval_file)
) {
  
  
  ### Utilities ----------------------------------------------------------------
  require(dplyr)
  require(ncdf4)
  
  
  
  ### Function inputs ----------------------------------------------------------
  site <- unlist(site_list)       # site of interest (parallized, also works for testing)
  
  savedata <- future_env[[1]]     # save or not
  eval_file <- future_env[[2]]    # evaluation file
  
  
  # Initialize txt vector for evaluation
  txt_vector <- c()
  
  
  
  ## Output by errors ----
  err_output <- tibble(SITE_ID = site, IGBP = NA_character_, LATITUDE = NA_real_, LONGITUDE = NA_real_) # output when error is encountered
  
  
  
  ### Import data --------------------------------------------------------------
  ## Import data for the current site
  txt <- glue::glue("++++++++++++++++++++++++++++ Site {site} ++++++++++++++++++++++++++++"); print(txt); txt_vector <- c(txt_vector, txt)
  txt <- glue::glue("....Importing data for site {site}."); print(txt); txt_vector <- c(txt_vector, txt)
  
  ## Fluxes
  nc_data <- tryCatch({
    ncdf4::nc_open(filename = glue::glue("{path}{site}.nc")) # fluxes
  }, error = function(err) {
    return(NA)
  })
  if (typeof(nc_data) == "logical") { # condition to exit computations for current site
    txt <- glue::glue("!=> Error in the import of flux data for site {site}. Returning NA for current site."); warning(txt); txt_vector <- c(txt_vector, txt)
    if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
    
    return(err_output)
    rlang::interrupt()
  }
  
  
  ## Extract variables ----
  dat_out <- tryCatch({
    tibble(
      ## Site:
      SITE_ID = site,
      ## Coordinates:
      LATITUDE = ncdf4::ncvar_get(nc_data, varid = "tower_lat"),  # latitude (degrees north)
      LONGITUDE = ncdf4::ncvar_get(nc_data, varid = "tower_lon")  # latitude (degrees east)
    ) %>%
      mutate(across(.cols = everything(), .fns = as.vector)) # convert every column type (array) to vector
    
  }, error = function(err) {
    return(NA)
  })
  if (typeof(dat_out) == "logical") { # condition to exit computations for current site
    txt <- glue::glue("!=> Error in the extraction of the coordinates for site {site}. Returning NA for current site."); warning(txt); txt_vector <- c(txt_vector, txt)
    if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
    
    return(err_output)
    rlang::interrupt()
  }
  
  
  
  ### Clean memory -------------------------------------------------------------
  rm(nc_data)
  gc() # clean memory usage
  
  
  
  ### Output -------------------------------------------------------------------
  txt <- glue::glue("==> Coordinates imported correctly for site {site}.")
  print(txt); txt_vector <- c(txt_vector, txt)
  if (savedata) {cat(paste0(txt_vector, "\n"), file = eval_file, append = T)} # print to evaluation file
  
  return(dat_out)
  
  
  
} # end of function



### Debugging ------------------------------------------------------------------
debugonce(import_COORDS)