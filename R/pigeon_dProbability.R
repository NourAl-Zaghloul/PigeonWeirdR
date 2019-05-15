pigeon_dProbability <- function(){}

#### Original Test Code ----
# library(tidyverse)
# 
# # Find the probability that a dice will roll higher than another
# # Probability off difering dice types and amounts
# # + Probability of "rounds"
# # + Advantage/Disadvantage probability(/equivolant modifier)
# 
# #### Testing ####
# 
# # creating a simple dice probability table
# d6 <- tibble(
#   result = 1:6,
#   probability = 1/length(result)
# )
# 
# #creating a multi-d probability table
# Twod6 <- tibble(
#   result1 = 1:6,
#   result2 = 1:6) %>% 
#   expand(result1,result2) %>% 
#   mutate(
#     result = result1 + result2) %>% 
#   group_by(result) %>% 
#   summarise(twod6 = n())
# 
# Threed6 <- tibble(
#   result1 = 1:6,
#   result2 = 1:6,
#   result3 = 1:6) %>% 
#   expand(result1,result2,result3) %>% 
#   mutate(
#     result = result1 + result2 + result3) %>% 
#   group_by(result) %>% 
#   summarise(threed6 = n())
# 
# result_temp <- full_join(Threed6, Twod6)
# result_temp[is.na(result_temp)] <- 0
# result_temp <- bind_rows(result_temp, data.frame(result = 0, threed6 = 0, twod6 = 0))
# result_temp <- result_temp[order(result_temp$result),]
# 
# xy_sum <- sum(result_temp$twod6)* sum(result_temp$threed6)
# 
# greater <- 0
# equal <- 0
# for(i in 1:(nrow(result_temp[result_temp$result < 6,])-1)){
#   greater <- greater + (pull(result_temp[i+1,3]) * sum(result_temp[result_temp$result<i+1,2]))
#   equal <- equal + (pull(result_temp[i+1,3]) * pull(result_temp[i+1,2]))
# }
# greater/(x_sum*y_sum)
# sum(equal)/(x_sum*y_sum)
# 
# 
# 
# #### general function ####
# pigeon_dicechance <- function(A = "1d6", B = NULL
#                               #, rounds = 1, advantage = c(0,0), output = "summary"
# ){
#   # if given only A, just a probability table
#   # if given A and B as dice, summary of >, =, <
#   # if given A as die and B as #, same as above
#   
#   A_vector <- as.integer(str_split(A, "[:alpha:]", simplify = TRUE))
#   B_vector <- as.integer(str_split(B, "[:alpha:]", simplify = TRUE))
#   N_vector <- c(A_vector[1], B_vector[1])
#   
#   # First create a function that makes dice tables
#   dicetable <- function(A,B){
#     for(i in 1:length(c(A,B))){
#       if(N_vector[i] == 1){
#         
#       } else if(N_vector[i] == 1){
#         
#       } else if(N_vector[i] == 2){
#         
#       } else if(N_vector[i] == 3){
#         
#       } else if(N_vector[i] == 4){
#         
#       } else if(N_vector[i] == 5){
#         
#       } else if(N_vector[i] == 6){
#         
#       } else if(N_vector[i] == 7){
#         
#       } else if(N_vector[i] == 8){
#         
#       } else if(N_vector[i] == 9){
#         
#       } else if(N_vector[i] == 10){
#         
#       } else if(N_vector[i] == 11){
#         
#       } else if(N_vector[i] == 12){
#         
#       } else if(N_vector[i] == 13){
#         
#       } else if(N_vector[i] == 14){
#         
#       } else if(N_vector[i] == 15){
#         
#       } else if(N_vector[i] == 16){
#         
#       } else if(N_vector[i] == 17){
#         
#       } else if(N_vector[i] == 18){
#         
#       } else if(N_vector[i] == 19){
#         
#       } else if(N_vector[i] == 20){
#         
#       } 
#       
#     }
#   }
#   
#   A_vector <- as.integer(str_split(A, "[:alpha:]", simplify = TRUE))
#   B_vector <- as.integer(str_split(B, "[:alpha:]", simplify = TRUE)) 
#   
#   #Use triangular numbers and 
#   
#   #WRONG
#   #This creates a variable amount of vectors but I don't know how to translate them to 
#   result_A <- matriA(rep(seq(as.integer(A[1,2])), as.integer(A[1,1])), 
#                      ncol = as.integer(A[1,1]))
#   
# }
# 
# Ur1 <- tibble(
#   r1 = 0:4,
#   r2 = r1,
#   r3 = r1,
#   r4 = r1
# ) %>% expand(r1,r2,r3,r4) %>% mutate(result = r1+r2+r3+r4) %>% group_by(result) %>% summarise(probs = n()) %>% mutate(probs = probs/5^4)
