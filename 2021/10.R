## Day 10

#### part 1 ####

library(stringr)
data_prep <- function(input){
  unlist(str_split(input, ""))
}

remove_pair <- function(x){
  x_in <- length(x)
  for (i in 1:(length(x)-1)){
    if (Position(function(y) identical(y, c(x[i],x[i+1])), good_pairs, nomatch = 0) > 0){
      x <- x[-c(i, i+1)]
      break
    } 
  }
  x_out <- length(x)
  x_diff <- (x_in - x_out) > 0
  list(x=x, x_diff=x_diff)
}

remove_pair_iter <- function(x){
  temp <- remove_pair(x)
  while(temp$x_diff){
    x <- temp$x
    temp <- remove_pair(x)
  }
  x
}

check_corrupt <- function(x){
  output <- remove_pair_iter(x)
  if (all(output %in% left) | all(output %in% right)){
    FALSE
  } else {
    return(output[which(output %in% right)[1]])
  }
}

#test
x1 <- read.delim("2021/Data/10_1_test.txt", header = F, sep = " ")
#answer
x1 <- read.delim("2021/Data/10_1.txt", header = F, sep = " ")

left <- c("[", "{", "<", "(")
right <- c("]", "}", ">", ")")
good_pairs <- list(c("[", "]"), c("(", ")"), c("{", "}"), c("<", ">"))

data_input <- apply(x1, 1, function(x) data_prep(x))
output <- unlist(lapply(data_input, function(x) check_corrupt(x)))
answer <- sum(table(output)*c(3, 57, 1197, 25137, 0))

#### part 2 ####

#retrn false versions
#mirror to complete
#get score
#sort scores
#middle value 

check_incomplete <- function(x){
  output <- remove_pair_iter(x)
  if (all(output %in% left) | all(output %in% right)){
    return(output)
  } 
}

value_sol <- function(output_vec){
  y <- rev(output_vec)
  score <- 0
  for (i in 1:length(y)){
    score <- score*5
    score <- score + val_table[which(val_table == y[i]),2]
  }
  score
}


#test
x1 <- read.delim("2021/Data/10_1_test.txt", header = F, sep = " ")

val_table <- data.frame(left, value=c(2, 3, 4, 1))

data_input <- apply(x1, 1, function(x) data_prep(x))
output <- lapply(data_input, function(x) check_incomplete(x))
output <- output[!sapply(output,is.null)]
scores <- unlist(lapply(output, function(x) value_sol(x)))
sort(scores)[(length(scores) + 1)/2]


#answer
x1 <- read.delim("2021/Data/10_1.txt", header = F, sep = " ")

val_table <- data.frame(left, value=c(2, 3, 4, 1))

data_input <- apply(x1, 1, function(x) data_prep(x))
output <- lapply(data_input, function(x) check_incomplete(x))
output <- output[!sapply(output,is.null)]
scores <- unlist(lapply(output, function(x) value_sol(x)))
sort(scores)[(length(scores) + 1)/2]




