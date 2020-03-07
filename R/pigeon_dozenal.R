#' No fractions yet
#'
#' @param x
#' @param base_from
#' @param base_to
pigeon_dozenal <- function(x, base_from = 10, base_to = 12){
  if(!is.numeric(base_from) || !is.numeric(base_to)){
    stop("Bases must be in numeric form (in base 10)", call. = FALSE)
  } else if(base_from < 2 || base_to < 2){
    stop("Cannot convert bases < 2", call. = FALSE)
  } else if(base_to > 62 || base_from > 62){
    stop("Cannot convert bases > 62", call. = FALSE)
  }

  # TODO: Vectorize the digit subs
  # TODO: Fractions using MASS::fractions() as a step
  # TODO: Error catching (e.g. base_to = 1, unknown bases, not ready functions,
  #       not numbers given for bases, larger base than we have notation for...)
  # TODO: Allow user to add in custom dictionaries for to/from digitsubs
  # TODO: Weird base systems (balanced, negative, complex, etc.)


  # Creating generic vector for 10 < baseX <= 62
  symbols_base <- c("`", "~", "!", "@", "#", "$", "%", "^", "&", "*", "\\(", "\\)",
                    "-", "_", "+", "=", "\\{", "\\[", "\\]", "\\}", "\\\\", "|", ":", ";",
                    "\"", "'", "<", ",", ">", ".", "?", "/")
  subdigits_62 <- c(0:9, LETTERS, letters)
  # subdigits_94 <- c(0:9, LETTERS, letters, symbols_base)

  #### Base_from conversion ----

  if(base_from != 10){

    x_convert <- unlist(strsplit(as.character(x), ""))
    x_temp <- 0

    if( base_from > 10 & base_from <= 62){
      ### Substitute + convert to decimal for bases > 10
      if(base_from == 12){

        x_convert <- gsub("X", 10, x_convert)
        x_convert <- gsub("E", 11, x_convert)

        } else if(base_from <= 62) {
        for(k in seq(11, base_from) ){
          x_convert <- gsub(subdigits_62[k], k, x_convert)
        }
      }
    }

    for(j in seq(length(x_convert) - 1)){

      x_temp <- ((as.integer(x_convert[j]) + x_temp) * base_from)
    }

    x <- x_temp + as.integer(x_convert[j+1])

  }


  x <- ifelse(x > 2147483647, as.numeric(x), as.integer(x))



  #### Actual Conversion processes (base_from = 10 to base_to positional) ----
  if(base_to == 10){
    OUT <- x
  } else {
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

    # Substituting the notation for base_to
    if(base_to == 12){
      OUT <- gsub(10, "X", OUT)
      OUT <- gsub(11, "E", OUT)

    } else if(base_to > 10 & base_to <= 62) {
      for(i in seq(base_from, base_to - 1) ){
        OUT <- gsub(i, subdigits_62[i+1], OUT)
      }
    }

    # Creating the "10" for every base
    OUT <- gsub(base_to, "10", OUT)

    # Collapsing the vector to a single value
    OUT <- paste0(OUT, collapse = "")
  }


  #### Return ----
  return(OUT)
}



