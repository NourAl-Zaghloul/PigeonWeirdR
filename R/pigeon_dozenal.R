#' Converts base10 integers to base12
#' Currently only works with base_from = 10 & (base_to = 12 or base_to <= 10)
#' No fractions yet
#'
#' @param x
#' @param base_from
#' @param base_to
pigeon_dozenal <- function(x, base_from = 10, base_to = 12){

  # TODO: Dictionaries for conversion for other (common) bases [12, 16, 20, 60]
  # TODO: Dictionary for uncommon bases c(0:9,Letters,letters, ?unicode)
  # TODO: Vectorize the digit subs
  # TODO: From any base to any base (using base 10 as an intermediary)
  # TODO: Fractions using MASS::fractions() to help
  # TODO: Allow user to add in custom dictionaries for to/from digitsubs
  # TODO: Weird base systems (balanced, negative, complex, etc.)

  #### Actual Conversion processes (base12 to base10 positional) ----
  i <- 1
  OUT <- c()
  quotient <- x
  if(quotient > base_to){

    while(quotient > base_to){
      OUT[i] <- quotient %% base_to
      i <- i + 1
      quotient <- trunc(quotient / base_to)
    }
    OUT[i] <- trunc(quotient)
    OUT <- rev(OUT)

  } else {
    OUT <- quotient
  }

  #### Subbing out base system numbers ----
  # base12 subs
  if(base_to == 12){
    OUT <- gsub(10, "X", OUT)
    OUT <- gsub(11, "E", OUT)
  } else if(base_to == 16){

  } else if(base_to == 20){

  } else if( base_to == 60){

  } else{
    subdigits <- c(0:9, LETTERS, letters) # TODO: use unicode for bases > 62
  }

  OUT <- gsub(base_to, "10", OUT)
  OUT <- paste0(OUT, collapse = "")

  return(OUT)
}



