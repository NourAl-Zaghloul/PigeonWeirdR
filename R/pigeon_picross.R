pigeon_picrossCreate <- function(x = 10, y = 10, seed = 112211, locations = .5){
  picross_fill <- c(rep(1, trunc(x*y*locations)), rep(0, x*y - trunc(x*y*locations) ))
  set.seed(seed)
  picross_fill <- sample(picross_fill)
  picross_inner <- matrix(picross_fill, nrow = y, ncol = x)
  return(picross_inner)
}

pigeon_picrossSolve <- function(width_values, height_values){
  #TODO: setup so that it runs all the processes by vector and not all the vectors by process
  #TODO: setup so that zero-created edges count as a smaller length

}

#### Setup the nonogram parameters ----
# Define nonogram parameters
# Solved Parameter:
# nonogram_width <- list(c(3,1),5,c(2,2),1,3)
# nonogram_height <- list(3, 3, c(2,2),c(2,1),c(3,1))
# More Difficult Parameters
nonogram_width <- list(3, c(1,2), 4, c(1,1), c(2,1))
nonogram_height <- list(0, 4, c(1,1,1), 3, 5)

# Creates empty nonogram matrix
nonogram <- matrix(nrow = length(nonogram_width),
                   ncol = length(nonogram_height))

#### 1. Solves the complete Rows [X] ----
# TODO: Remove overlap between width & height complete rows
for(i in seq(length(nonogram_width))){
  if(NA %in% nonogram[, i] && (sum(nonogram_width[[i]]) + length(nonogram_width[[i]]) - 1) == length(nonogram_width)){
    # Creates the completed vector
    answer <- NULL
    for(j in seq(length(nonogram_width[[i]]))){
      answertemp <- c(rep(1, nonogram_width[[i]][j]), 0)
      answer <- c(answer, answertemp)
    }
    answer <- answer[1:length(nonogram_width)]
    nonogram[, i] <- answer
  } else if(sum(nonogram_width[[i]]) == 0){
    nonogram[, i] <- rep(0, length(nonogram_width))
  }
}

for(i in seq(length(nonogram_height))){
  if(NA %in% nonogram[i, ] && (sum(nonogram_height[[i]]) + length(nonogram_height[[i]]) - 1) == length(nonogram_height)){
    # Creates the completed vector
    answer <- NULL
    for(j in seq(length(nonogram_height[[i]]))){
      answertemp <- c(rep(1, nonogram_height[[i]][j]), 0)
      answer <- c(answer, answertemp)
    }
    answer <- answer[1:length(nonogram_height)]
    nonogram[i, ] <- answer
  } else if(sum(nonogram_height[[i]]) == 0){
    nonogram[i, ] <- rep(0, length(nonogram_height))
  }
}


#### 2. Adds necessary locations [X] ----
#. e.g. matrix = 5x5 and clue = "4", == c(NA, 1,1,1, NA)

for(i in seq(length(nonogram_width))){
  # Determines the value clues need to exceed to have necessary locations in the answer
  pic_diff <- length(nonogram[, i]) - (sum(nonogram_width[[i]]) + length(nonogram_width[[i]]) - 1)
  # Checks to see if the vector is unsolved && a clue exceeds the pic_diff
  if(NA %in% nonogram[, i] && pic_diff < max(nonogram_width[[i]])){
    pic_vector <- nonogram[, i]
    # Creates the offsetting for the necessary locations
    pic_base <- 1
    #Creates the nec. locations for each clue if applicable
    for(j in seq(length(nonogram_width[[i]]))){
      if(nonogram_width[[i]][j] > pic_diff){
        pic_vector[(pic_base+pic_diff):nonogram_width[[i]][j]] <- 1
      }
      pic_base <- pic_base + nonogram_width[[i]][j] + 1
    }
    nonogram[, i] <- pic_vector
  }
}

for(i in seq(length(nonogram_height))){
  # Determines the value clues need to exceed to have necessary locations in the answer
  pic_diff <- length(nonogram[i, ]) - (sum(nonogram_height[[i]]) + length(nonogram_height[[i]]) - 1)
  # Checks to see if the vector is unsolved && a clue exceeds the pic_diff
  if(NA %in% nonogram[i, ] && pic_diff < max(nonogram_height[[i]])){
    pic_vector <- nonogram[i, ]
    # Creates the offsetting for the necessary locations
    pic_base <- 1
    #Creates the nec. locations for each clue if applicable
    for(j in seq(length(nonogram_height[[i]]))){
      if(nonogram_height[[i]][j] > pic_diff){
        pic_vector[(pic_base+pic_diff):nonogram_height[[i]][j]] <- 1
      }
      pic_base <- pic_base + nonogram_height[[i]][j] + 1
    }
    nonogram[i, ] <- pic_vector
  }
}


#### 3. Solves edge-cases [ ~] ----
#. e.g. c(0,0,NA,NA,NA) for clue "3" | c(NA,0,1,NA,NA) for clue "3" | c(1, NA, NA, NA, NA) for clue "3"

# TODO: count end 0's as edges as well

for(i in seq(length(nonogram_width))){
  # Creates the vector string and its reverse
  TheString <- stringr::str_replace_na(nonogram[,i], replacement = "_")
  TheString <- stringr::str_c(TheString, collapse = "")
  pic_start <- nonogram_width[[i]][1]
  pic_end <- nonogram_width[[i]][length(nonogram_width[[i]])]

  if(all(c(NA,1) %in% nonogram[, i]) && pic_start > stringr::str_locate(TheString, "1")[1,1]){
    answertemp <- nonogram[, i]
    answertemp[stringr::str_locate(TheString, "1")[1,1]:(pic_start+1)] <- c(rep(1, length(stringr::str_locate(TheString, "1")[1,1]:pic_start)),0)
    nonogram[, i] <- answertemp[seq(length(nonogram[, i]))]
  } else if(all(c(NA,1) %in% nonogram[, i]) && pic_end > (stringr::str_locate(TheString,"1*$")[1,2] - length(nonogram[, i]) + 1)){
    answertemp <- rev(nonogram[, i])
    answertemp[(stringr::str_locate(TheString,"1*$")[1,2] - length(nonogram[, i]) + 1):(pic_end+1)] <- c(rep(1, length((stringr::str_locate(TheString,"1*$")[1,2] - length(nonogram[, i]) + 1):(pic_end))),0)
    nonogram[, i] <- rev(answertemp[seq(length(nonogram[, i]))])
  }
  rm(pic_start, pic_end, answertemp, TheString)
}

for(i in seq(length(nonogram_height))){
  # Creates the vector string and its reverse
  TheString <- stringr::str_replace_na(nonogram[,i], replacement = "_")
  TheString <- stringr::str_c(TheString, collapse = "")
  pic_start <- nonogram_height[[i]][1]
  pic_end <- nonogram_height[[i]][length(nonogram_height[[i]])]

  if(all(c(NA,1) %in% nonogram[i, ]) && pic_start > stringr::str_locate(TheString, "1")[1,1]){
    answertemp <- nonogram[i, ]
    answertemp[stringr::str_locate(TheString, "1")[1,1]:(pic_start+1)] <- c(rep(1, length(stringr::str_locate(TheString, "1")[1,1]:pic_start)),0)
    nonogram[i, ] <- answertemp[seq(length(nonogram[i, ]))]
  } else if(all(c(NA,1) %in% nonogram[i, ]) && pic_end > (stringr::str_locate(TheString,"1*$")[1,2] - length(nonogram[i, ]) + 1)){
    answertemp <- rev(nonogram[i, ])
    answertemp[(stringr::str_locate(TheString,"1*$")[1,2] - length(nonogram[i, ]) + 1):(pic_end+1)] <- c(rep(1, length((stringr::str_locate(TheString,"1*$")[1,2] - length(nonogram[i, ]) + 1):(pic_end))),0)
    nonogram[i, ] <- rev(answertemp[seq(length(nonogram[i, ]))])
  }
  rm(pic_start, pic_end, answertemp, TheString)
}


#### 4. 0's impossible locations [ ] ----
#. values too far away, not enough space, etc.
# TODO: this.

#### 5. Solves single in-between (single value vectors only currently) [~] ----
#. e.g. for the clue "3" and vector c(1,NA,1,NA,NA) <- c(1,1,1,0,0)
for(i in seq(ncol(nonogram))){
  TheString <- stringr::str_replace_na(nonogram[,i])
  TheString <- stringr::str_c(TheString, collapse = "")
  if(NA %in% nonogram[,i] && length(nonogram_width[[i]]) == 1 & grepl("1(NA)+1",TheString)){
    FillLocation <- grep(1, nonogram[,i])
    for(j in FillLocation[1]:FillLocation[2]){
      nonogram[j,i] <- 1
    }
  }
}

for(i in seq(nrow(nonogram))){
  TheString <- stringr::str_replace_na(nonogram[i,])
  TheString <- stringr::str_c(TheString, collapse = "")
  if(NA %in% nonogram[,i] && length(nonogram_height[[i]]) == 1 & grepl("1(NA)+1",TheString)){
    FillLocation <- grep(1, nonogram[i,])
    for(j in FillLocation[1]:FillLocation[2]){
      nonogram[i,j] <- 1
    }
  }
}


#### X-1. Completes teriary-complete vectors [ ] ----
#. Vectors that were completed as a by-product between the true complete vectors
for(i in seq(ncol(nonogram))){
  if(NA %in% nonogram[,i] & sum(nonogram[,i], na.rm = TRUE) == sum(nonogram_width[[i]])){
    nonogram[,i][is.na(nonogram[,i])] <- 0
  }
}

for(i in seq(nrow(nonogram))){
  if(NA %in% nonogram[i,] & sum(nonogram[i,], na.rm = TRUE) == sum(nonogram_height[[i]])){
    nonogram[i,][is.na(nonogram[i,])] <- 0
  }
}

#### X. Runs through permutations of possible solutions [ ] ----
#. Bogosolve with the leftovers basically
# TODO: This.



#### Z. Miscellaneous Code Bits & Ideas ----
traveller <- "0011NA"
stringr::str_count(traveller, "0|(NA)")
