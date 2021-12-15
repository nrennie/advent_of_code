## Day 9
library(igraph)
library(stringr)

#### part 1 ####
day_9_1 <- function(input){
  output <- numeric()
  for (i in 1:nrow(input)){
    for (j in 1:ncol(input)){
      
      #find other positions
      up <- input[i-1,j]
      if (i < nrow(input)){
        down <- input[i+1,j]
      } else {
        down <- c()
      }
      left <- input[i, j-1]
      if (j < ncol(input)){
        right <- input[i, j+1]
      } else {
        right <- c()
      }
      
      #check if these are defined
      valid_pos <- c(up, down, left, right)
      
      #check if low point
      if (all(input[i,j] < valid_pos)){
        output <- c(output, input[i,j])
      }
    }
  }
  sum(output + 1)
}

#test 
x1 <- read.fwf("2021/Data/9_1_test.txt", widths=10)
input <- t(apply(x1, 1, function(x) as.numeric(unlist(str_split(x, "")))))
day_9_1(input)

#answer
x1 <- read.fwf("2021/Data/9_1.txt", widths=100, colClasses = c("character"))
input <- t(apply(x1, 1, function(x) as.numeric(unlist(str_split(x, "")))))
day_9_1(input)

#### part 2 ####

#test
x1 <- read.fwf("2021/Data/9_1_test.txt", widths=10)

#answer
x1 <- read.fwf("2021/Data/9_1.txt", widths=100, colClasses = c("character"))
input <- t(apply(x1, 1, function(x) as.numeric(unlist(str_split(x, "")))))
bin_input  <- 1*(input < 9)

#make graph
g_df <- matrix(0, ncol=length(bin_input), nrow=length(bin_input))

#name rows and columns
rownames(g_df) <- apply(expand.grid(1:nrow(bin_input), 1:ncol(bin_input)), 1, function(k) str_c(k, sep="", collapse=","))
colnames(g_df) <- apply(expand.grid(1:nrow(bin_input), 1:ncol(bin_input)), 1, function(k) str_c(k, sep="", collapse=","))

for (i in 1:nrow(bin_input)){
  for (j in 1:ncol(bin_input)){
    if(bin_input[i,j] == 1){
      
      #up
      up <- bin_input[i-1,j]
      if (length(up) > 0){
        if (up == 1){
          g_df[str_c(c(i,j), sep="", collapse=","), str_c(c(i-1,j), sep="", collapse=",")] <- 1
        }
      }

      #down
      if (i < nrow(bin_input)){
        down <- bin_input[i+1,j]
      } else {
        down <- c()
      }
      if (length(down) > 0){
        if (down == 1){
          g_df[str_c(c(i,j), sep="", collapse=","), str_c(c(i+1,j), sep="", collapse=",")] <- 1
        }
      }
      
      #left
      left <- bin_input[i, j-1]
      if (length(left) > 0){
        if (left == 1){
          g_df[str_c(c(i,j), sep="", collapse=","), str_c(c(i,j-1), sep="", collapse=",")] <- 1
        }
      }
      
      #right
      if (j < ncol(bin_input)){
        right <- bin_input[i, j+1]
      } else {
        right <- c()
      }
      if (length(right) > 0){
        if (right == 1){
          g_df[str_c(c(i,j), sep="", collapse=","), str_c(c(i,j+1), sep="", collapse=",")] <- 1
        }
      }
      
    }
    
  }
}

#make graph
g <- graph_from_adjacency_matrix(g_df, mode="undirected")
com <- components(g)
prod(tail(sort(com$csize), 3))




