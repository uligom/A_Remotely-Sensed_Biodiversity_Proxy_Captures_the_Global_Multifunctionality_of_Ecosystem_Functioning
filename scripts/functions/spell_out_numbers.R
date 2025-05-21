spell_out_numbers <- function(d, caps = F) {
  require(dplyr)
  
  if (caps == F) {
    spelled_out <- case_when(
      d == 1 ~ "one",
      d == 2 ~ "two",
      d == 3 ~ "three",
      d == 4 ~ "four",
      d == 5 ~ "five",
      d == 6 ~ "six",
      d == 7 ~ "seven",
      d == 8 ~ "eight",
      d == 9 ~ "nine",
      d == 10 ~ "ten",
      T ~ as.character(d)
    )
  } else if (caps == T) {
    spelled_out <- case_when(
      d == 1 ~ "One",
      d == 2 ~ "Two",
      d == 3 ~ "Three",
      d == 4 ~ "Four",
      d == 5 ~ "Five",
      d == 6 ~ "Six",
      d == 7 ~ "Seven",
      d == 8 ~ "Eight",
      d == 9 ~ "Nine",
      d == 10 ~ "Ten",
      T ~ as.character(d)
    )
  }
  
  return(spelled_out)
}

# debugonce(spell_out_numbers)