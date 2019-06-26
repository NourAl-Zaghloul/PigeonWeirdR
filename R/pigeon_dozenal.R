#' Converts base10 integers to base12
#' Currently only works with base_from = 10 & (base_to = 12 or base_to <= 10)
#' No fractions yet
#'
#' @param x
#' @param base_from
#' @param base_to
pigeon_dozenal <- function(x, base_from = 10, base_to = 12){

  # TODO: Vectorize the digit subs
  # TODO: From any base to any base (using base 10 as an intermediary)
  # TODO: Fractions using MASS::fractions() as a step
  # TODO: Error catching (e.g. base_to = 1, unknown bases, not ready functions,
  #       not numbers given for bases, larger base than we have notation for...)
  # TODO: Make it pipeable
  # TODO: Dictionary for bases > 62
  # TODO: Allow user to add in custom dictionaries for to/from digitsubs
  # TODO: Weird base systems (balanced, negative, complex, etc.)

  #### Actual Conversion processes (base_from = 10 to base_to positional) ----
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

  # Creating generic vector for 10 < baseX <= 62
  subdigits_62 <- c(0:9, LETTERS, letters)

  # Creating the "10" for every base
  OUT <- gsub(base_to, "10", OUT)

  # Substituting the notation for base_to
  if(base_to == 12){
    OUT <- gsub(10, "X", OUT)
    OUT <- gsub(11, "E", OUT)

  } else if(base_to > 10 & base_to <= 62) {
    for(i in seq(base_from, base_to - 1) ){
      OUT <- gsub(i, subdigits_62[i+1], OUT)
    }
  }

  # Collapsing the vector to a single value
  OUT <- paste0(OUT, collapse = "")

  #### Return ----
  return(OUT)
}



