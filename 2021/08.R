## Day 8
library(stringr)

#### part 1 ####
count_1478 <- function(input){
  length(which(nchar(input) %in% c(2,3,4,7)))
}
count_1478(input)

#test
x1 <- read.delim("2021/Data/8_1_test.txt", header = F, sep = " ")
input <- x1[,12:15]
sum(apply(input, 1, function(x) count_1478(x)))

#answer
x1 <- read.delim("2021/Data/8_1.txt", header = F, sep = " ")
input <- x1[,12:15]
sum(apply(input, 1, function(x) count_1478(x)))

#### part 2 ####

#string sort function
str_sort <- function(str){
  str_flatten(sort(unlist(str_split(str, ""))), "")
}
str_sort("ba")

#need to figure out from input
value_match <- function(input){
  sorted_input <- unlist(lapply(input, function(x) str_sort(x)))
  output_df <- data.frame(str=rep(NA_character_, 10), val=0:9)
  #1
  output_df[2, 1] <- sorted_input[which(nchar(sorted_input) == 2)]
  #4 
  output_df[5, 1] <- sorted_input[which(nchar(sorted_input) == 4)]
  #7
  output_df[8, 1] <- sorted_input[which(nchar(sorted_input) == 3)]
  #8
  output_df[9, 1] <- sorted_input[which(nchar(sorted_input) == 7)]
  
  #five character (2,3,5)
  options_5 <- sorted_input[which(nchar(sorted_input) == 5)]
  #six characters (6,9,0)
  options_6 <- sorted_input[which(nchar(sorted_input) == 6)]
  
  #3 
  k3 <- which(grepl(unlist(str_split(output_df[2, 1], ""))[1], options_5) + 
                grepl(unlist(str_split(output_df[2, 1], ""))[2], options_5) == 2)
  output_df[4, 1] <- options_5[k3]
  
  #9 
  k9 <- str_sort(str_flatten(union(unlist(str_split(output_df[4,1],"")), unlist(str_split(output_df[5,1],""))), ""))
  output_df[10, 1] <- options_6[which(options_6 == k9)]
  
  #0 
  new_options_6 <- options_6[which(options_6 != k9)]
  k0 <- unlist(lapply(1:2, function(x) all(unlist(str_split(output_df[8, 1], "")) %in% unlist(str_split(new_options_6[x], "")))))
  output_df[1, 1] <- new_options_6[k0]
  
  #6
  output_df[7, 1] <- new_options_6[!k0]
  
  #5
  new_options_5 <- options_5[-k3] 
  k5 <- unlist(lapply(1:2, function(x) all(unlist(str_split(new_options_5[x], "")) %in% unlist(str_split(output_df[7, 1], "")))))
  output_df[6, 1] <- new_options_5[k5]
  
  #2
  output_df[3, 1] <- new_options_5[!k5]
  
  return(output_df)
}

get_value <- function(input_df){
  look_up <- value_match(input_df[1:10])
  output <- unlist(lapply(input_df[12:15], function(x) str_sort(x)))
  as.numeric(str_flatten(as.character(unlist(lapply(output, function(x) look_up[look_up$str == x, 2]))), ""))
}

#test
x1 <- read.delim("2021/Data/8_1_test.txt", header = F, sep = " ")
tot <- 0
for (i in 1:nrow(x1)){
  tot <- tot + get_value(x1[i,])
}
  
#answer
x1 <- read.delim("2021/Data/8_1.txt", header = F, sep = " ")
tot <- 0
for (i in 1:nrow(x1)){
  tot <- tot + get_value(x1[i,])
}











