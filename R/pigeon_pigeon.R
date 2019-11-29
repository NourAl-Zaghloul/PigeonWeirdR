pigeon_pigeon <- function(){
  pigeon_species <- readRDS("pigeon_species.RDS")
  pigeon_breeds <- readRDS("pigeon_breeds.RDS")

  return(pigeon_species$`Common name`[trunc(runif(1,min = 1, max = 100))])

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
