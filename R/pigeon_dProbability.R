pigeon_dProbability <- function(x, y = NA, operation = NA){
  library(tidyverse)

  # TODO: Operations
  # TODO: Arguments
  # TODO: assume "dN" == "1dN"
  # TODO: Custom Dice

  #### Dice X Information ----
  dicetype_x <- stringr::str_extract(x, "(?<=[:digit:])[:alpha:]+")
  dicequant_x <- as.numeric(stringr::str_extract(x, ".+(?=[:alpha:])"))
  dicesize_x <- as.numeric(stringr::str_extract(x, "(?<=[:alpha:]).+"))
  dicearg_x <- stringr::str_extract(x, "(?<=[:digit:]_).+")

  dicetable_x_slug <- data.frame(rep(NA, dicesize_x))

  for(i in seq(dicequant_x)){
    dicetable_x_slug[, i] <- seq(dicesize_x)
  }

  colnames(dicetable_x_slug) <- paste0("die",seq(dicequant_x))
  dicetable_x_full <- expand.grid(dicetable_x_slug)
  dicetable_x_full$total <- rowSums(dicetable_x_full, na.rm = TRUE)
  dicetable_x_sum <- dicetable_x_full %>%
    group_by(total) %>%
    summarise(x_percentage = n()/length(dicetable_x_full$total) * 100,
              x_count = n())
  # TODO: Arguments
  #   keep/drop({N}Highest/Lowest), +/-N, +/-d


  #### Dice Y Information ----
  if(!is.na(y)){
    dicetype_y <- stringr::str_extract(y, "(?<=[:digit:])[:alpha:]+")
    dicequant_y <- as.numeric(stringr::str_extract(y, ".+(?=[:alpha:])"))
    dicesize_y <- as.numeric(stringr::str_extract(y, "(?<=[:alpha:]).+"))
    dicearg_y <- stringr::str_extract(y, "(?<=[:digit:]_).+")

    dicetable_y_slug <- data.frame(rep(NA, dicesize_y))

    for(i in seq(dicequant_y)){
      dicetable_y_slug[, i] <- seq(dicesize_y)
    }

    colnames(dicetable_y_slug) <- paste0("die",seq(dicequant_y))
    dicetable_y_full <- expand.grid(dicetable_y_slug)
    dicetable_y_full$total <- rowSums(dicetable_y_full, na.rm = TRUE)
    dicetable_y_sum <- dicetable_y_full %>%
      group_by(total) %>%
      summarise(y_percentage = n()/length(dicetable_y_full$total) * 100,
                y_count = n())

    #### Operation ----
    if(!is.na(operation)){
      # ==, <, >, >=, <=

    } else {

      # Assumes x > y
      dicetable_xy_sum <- dplyr::full_join(dicetable_x_sum, dicetable_y_sum,) %>%
        replace(., is.na(.), 0)
      for(i in seq(nrow(dicetable_xy_sum))){
        dicetable_xy_sum$x_over[i] <- sum(dicetable_xy_sum$x_percentage[(i+1):nrow(dicetable_xy_sum)])

      }

    }

  } else {
    return(dicetable_x_sum)
  }
}
