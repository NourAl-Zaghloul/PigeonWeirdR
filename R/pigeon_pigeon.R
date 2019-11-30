pigeon_pigeon <- function(x = 1, type = "any"){
  pigeon_species <- readRDS("pigeon_species.RDS")
  pigeon_breeds <- readRDS("pigeon_breeds.RDS")

  if(type == "any"){
    pigeon_data <- c(pigeon_breeds, pigeon_species$`Common name`)
  } else if(type == "breeds"){
    pigeon_data <- pigeon_breeds
  } else if(type == "species"){
    pigeon_data <- pigeon_species$`Common name`
  }

  return(pigeon_data[trunc(runif(x,min = 1, max = length(pigeon_data)+.999))])

}


#### Goals ------

# To Make a package that returns random pigeons
# Also, adjective phrases with pigeons
# Sentences?

#### Getting the data ----
# library(rvest)
# library(tidyverse)
#
# pigeon_html <- read_html("https://en.wikipedia.org/wiki/List_of_wild_pigeon_species")
# pigeon_data_species <- html_table(pigeon_html, fill = TRUE)[[1]]
# saveRDS(pigeon_data_species, file = "pigeon_species.RDS")
#
# pigeon2_html <- read_html("https://en.wikipedia.org/wiki/List_of_pigeon_breeds")
# pigeon_data_breed <- html_nodes(pigeon2_html, css = "ul li") %>% html_text()
# pigeon_data_breed <- pigeon_data_breed[27:750]
# saveRDS(pigeon_data_breed, file = "pigeon_breeds.RDS")
