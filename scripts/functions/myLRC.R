### Light Response Curve function for calculations (Mirco Migliavacca)
# added warning handling for predominant NA content (Ulisse Gomarasca, David Martini)

# Calculate light response curve fit:
myLRC <- function(data) {
  require(bigleaf)
  require(generics)
  require(rlang)
  
  if (sum(is.na(data$NEE))/length(data$NEE) >= 0.8) {
    warning("More than 80% of the data are NA. Not calculating light response curve and returning NA.")
    return(NA)
  } else if (sum(is.na(data$NEE))/length(data$NEE) < 0.8) {
    result = tryCatch({
      fitLRC <- bigleaf::light.response(as.data.frame(data), NEE = "NEE", Reco = "RECO", PPFD = "PPFD",
                               PPFD_ref = 2000) # variable names changed to this format in temporary input dataframe
      # out <- tidy(fitLRC)$estimate[2] # extract GPPsat estimate [as in original code]
      out <- summary(fitLRC)[["coefficients"]][2] # extract GPPsat estimate
      return(out)
    }, error = function(err) {
      warning("Error in the light response curve function. Returning NA.")
      return(NA_real_)
    })
  }
}