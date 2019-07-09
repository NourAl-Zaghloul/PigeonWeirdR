#' Collection of Recipes / Recipe Searcher
#'
#' @param
#' @param
pigeon_recipes <- function(food){
  recipe_vector <- c("falafal",
                   "hummus",
                   "fatayer",
                   "heshway",
                   "mtabal",
                   "madammas",
                   "sfiha",
                   "armenian sfiha",
                   "kabobs",
                   "knafeh",
                   "3arayes",
                   "CPSPHP")

  if(is.numeric(food)){
    if(food > length(recipe_vector)){
      recipe_vector <- rep(recipe_vector, length_out = food)
    }
    food <- recipe_vector[food]
  } else if(food == "random"){
    random <- sample(seq(length(recipe_vector)),1)
    food <- recipe_vector[random]
  }

  return(food)

}

#### TODO ----
# Pixel/ASCII art of the food
# "TacoBell Setting" which displays foods that share many of the same ingredients
# Actual ingredients (+Quantities in grams)
# Actual Recipe Step
# A "Tag" system
# Pricing/Cost + Servings
# Nutritional Information
