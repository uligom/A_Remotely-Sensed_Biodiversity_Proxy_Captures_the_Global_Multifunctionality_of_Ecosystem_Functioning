#### FUNCTION TO FORMAT NUMBERS WITH 2 DIGITS RIGHT OF ZERO
format_digits <- function(data = dat, y_var = "GPPsat", x_var = NULL, info = "estimate", mult = 1, digits = 2) {
  require(dplyr)
  require(magrittr)
  require(rlang)
  
  info <- rlang::sym(info) # quote
  
  data <- data %>% dplyr::filter(prediction == y_var) # filter EFP
  
  if (!is.null(x_var)) {
    data <- data %>% dplyr::filter(variable == x_var) %>% dplyr::pull(!!info) # filter predictor
  } else if (is.null(x_var)) {
    data <- data %>% dplyr::pull(!!info) %>% unique() # extract unique
  }
  
  out <- data %>% 
    magrittr::multiply_by(mult) %>% 
    round(digits = digits) %>% 
    format(nsmall = digits)
  
  return(out)
}
# debugonce(format_digits)