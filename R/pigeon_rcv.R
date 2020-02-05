# pigeon_rcv

#### testing concept ----
library(tidyverse)
votingdata <- read.csv("pigeonvotes.csv", stringsAsFactors = FALSE)
votingdata$voteCurrent <- votingdata$choiceFirst

# Counts first choice candidates
pass0 <- votingdata %>%
  group_by(voteCurrent) %>%
  summarise(votes = n(),
            percent = n()/nrow(votingdata)) %>%
  arrange(desc(votes))

# If anyone got 50% or more of the vote, they won!
#   else we need to remove the lowest votes and count again
if (max(pass0$percent) >= .5) {
  print(paste(pass0$choiceFirst[1], "is the winner!"))
} else {
  print("Needs a runoff.")
}
# TODO: Count if amount we would be dropping would be the majority
passFilter <- pass0$voteCurrent[pass0$votes == min(pass0$votes)]
votingdata <- votingdata %>%
  mutate(voteCurrent = case_when(!(voteCurrent %in% passFilter) ~ voteCurrent,
                                 TRUE ~ choiceSecond))
# repeat ad nasuem...

#### looping, simple
votingdata <- read.csv("pigeonvotes.csv") %>%
  mutate(voteCurrent = choiceFirst)
# TODO: Figure this shit out
#   for each row (voter) take the first vote that isn't already eliminated
#     [!row %in% passFilter][1]
#   check to see if anybody won
#     return winner
#   else calculate the new eliminated votes from the new totals
#   repeat


#### function ----
pigeon_rcv <- function(x){

  #### Error/Warning Catching ----
  if(!(class(x) %in% c("data.frame", "matrix"))){
    errorCondition("Data needs to be class dataframe")
  } else if(class(x) == "matrix"){
    x <- as.data.frame(x)
  }

  #### First Choice ----


}
