pigeon_dProbability <- function(x, y = NA, operation = NA){
  # library(tidyverse)

  # TODO: Arguments
  # TODO: assume "dN" == "1dN"
  # TODO: Flat # instead of die ( x < 6)
  # TODO: Be more DRY and better organized
  # TODO: Custom Dice
  # TODO: Explicit function calls

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
      if(operation == "==" || operation == "="){

        dicetable_xy_sum <- dplyr::full_join(dicetable_x_sum, dicetable_y_sum,) %>%
          replace(., is.na(.), 0) %>%
          mutate(endprobability = y_percentage * x_percentage / 100)
        OUT <- sum(dicetable_xy_sum$endprobability, na.rm = TRUE)

      } else if(operation == ">"){

        dicetable_xy_sum <- dplyr::full_join(dicetable_x_sum, dicetable_y_sum,) %>%
          replace(., is.na(.), 0)
        for(i in seq(nrow(dicetable_xy_sum))){
          dicetable_xy_sum$endprobability[i] <-
            sum(dicetable_xy_sum$x_percentage[(i+1):nrow(dicetable_xy_sum)]) *
            dicetable_xy_sum$y_percentage[i]
        }
        OUT <- sum(dicetable_xy_sum$endprobability, na.rm = TRUE)/100

      } else if(operation == "<"){

        dicetable_xy_sum <- dplyr::full_join(dicetable_x_sum, dicetable_y_sum,) %>%
          replace(., is.na(.), 0)
        for(i in seq(nrow(dicetable_xy_sum))){
          dicetable_xy_sum$endprobability[i] <-
            sum(dicetable_xy_sum$x_percentage[i:nrow(dicetable_xy_sum)]) *
            dicetable_xy_sum$y_percentage[i]
        }
        # Just 1 - the ">=" operation
        OUT <- 100 - sum(dicetable_xy_sum$endprobability, na.rm = TRUE)/100

      } else if(operation == ">="){

        dicetable_xy_sum <- dplyr::full_join(dicetable_x_sum, dicetable_y_sum,) %>%
          replace(., is.na(.), 0)
        for(i in seq(nrow(dicetable_xy_sum))){
          dicetable_xy_sum$endprobability[i] <-
            sum(dicetable_xy_sum$x_percentage[i:nrow(dicetable_xy_sum)]) *
            dicetable_xy_sum$y_percentage[i]
        }
        OUT <- sum(dicetable_xy_sum$endprobability, na.rm = TRUE)/100

      } else if(operation == "<="){

        dicetable_xy_sum <- dplyr::full_join(dicetable_x_sum, dicetable_y_sum,) %>%
          replace(., is.na(.), 0)
        for(i in seq(nrow(dicetable_xy_sum))){
          dicetable_xy_sum$endprobability[i] <-
            sum(dicetable_xy_sum$x_percentage[(i+1):nrow(dicetable_xy_sum)]) *
            dicetable_xy_sum$y_percentage[i]
        }
        # Just 1 - the ">" operation
        OUT <- 100 - sum(dicetable_xy_sum$endprobability, na.rm = TRUE)/100

      } else if(operation == "!="){

        dicetable_xy_sum <- dplyr::full_join(dicetable_x_sum, dicetable_y_sum,) %>%
          replace(., is.na(.), 0) %>%
          mutate(endprobability = (100 - y_percentage) * x_percentage / 100)
        OUT <- sum(dicetable_xy_sum$endprobability, na.rm = TRUE)
      }

    } else {

      # Assumes x == y
      dicetable_xy_sum <- dplyr::full_join(dicetable_x_sum, dicetable_y_sum,) %>%
        replace(., is.na(.), 0) %>%
        mutate(endprobability = y_percentage * x_percentage / 100)
      OUT <- sum(dicetable_xy_sum$endprobability, na.rm = TRUE)

    }

    return(abs(OUT))

  } else {
    return(dicetable_x_sum)
  }
}
