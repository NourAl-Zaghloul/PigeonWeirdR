pigeon_picrossCreate <- function(x = 10, y = 10, seed = 112211, locations = .5){
  picross_fill <- c(rep(1, trunc(x*y*locations)), rep(0, x*y - trunc(x*y*locations) ))
  set.seed(seed)
  picross_fill <- sample(picross_fill)
  picross_inner <- matrix(picross_fill, nrow = y, ncol = x)
  return(picross_inner)
}

pigeon_picrossSolve <- function(width_values, height_values){

}

#### Setup the nonogram parameters ----
# Define nonogram parameters
# Solved Parameter:
# nonogram_width <- list(c(3,1),5,c(2,2),1,3)
# nonogram_height <- list(3, 3, c(2,2),c(2,1),c(3,1))
# More Difficult Parameters
nonogram_width <- list(c(1,1),1,c(1,2),1,3)
nonogram_height <- list(c(1,1,1),1,c(1,1),4,0)

# Creates empty nonogram matrix
nonogram <- matrix(nrow = length(nonogram_width),
                   ncol = length(nonogram_height))

#### Solves the complete Rows ----
# TODO: Remove overlap between width & height complete rows
for(i in seq(length(nonogram_width))){
  if((sum(nonogram_width[[i]]) + length(nonogram_width[[i]]) - 1) == length(nonogram_width)){
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
  if((sum(nonogram_height[[i]]) + length(nonogram_height[[i]]) - 1) == length(nonogram_height)){
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

#### Completes teriary-complete vectors ----
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

#### Solves single in-between single value vectors
#. e.g. for the clue "3" and vector c(1,NA,1,NA,NA) <- c(1,1,1,0,0)
for(i in seq(ncol(nonogram))){
  TheString <- stringr::str_replace_na(nonogram[,i])
  TheString <- stringr::str_c(TheString, collapse = "")
  if(NA %in% nonogram[,i] & length(nonogram_width[[i]]) == 1 & grepl("1[NA]+1",TheString)){
    FillLocation <- grep(1, nonogram[,i])
    for(j in FillLocation[1]:FillLocation[2]){
      nonogram[j,i] <- 1
      }
  }
}

for(i in seq(nrow(nonogram))){
  TheString <- stringr::str_replace_na(nonogram[i,])
  TheString <- stringr::str_c(TheString, collapse = "")
  if(NA %in% nonogram[,i] & length(nonogram_height[[i]]) == 1 & grepl("1[NA]+1",TheString)){
    FillLocation <- grep(1, nonogram[i,])
    for(j in FillLocation[1]:FillLocation[2]){
      nonogram[i,j] <- 1
    }
  }
}

##3# Solves single edge-cases ----
#. e.g. c(0,0,NA,NA,NA) for clue "3" | c(NA,0,1,NA,NA) for clue "3"
#TODO: this.

#### 0's impossible locations ----
#. values too far away, not enough space, etc.
# TODO: this.

#### Adds in necessary locations (but not completed rows) ----
#. e.g. matrix = 5x5 and clue = "4", == c(NA, 1,1,1, NA)
# TODO: this.

#### runs through permutations of possible solutions ----
#. Bogosolve with the leftovers basically
# TODO: This.

