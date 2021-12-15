## Day 4

#### part 1 ####

day_4_1 <- function(input_boards){
  output_boards <- list()
  
  for (i in 1:length(input_boards)){
    output_boards[[i]] <- matrix(0, nrow=5, ncol=5)
  }
  
  for (i in 1:length(bingo_nums)){
    for (j in 1:length(input_boards)){
      d <- input_boards[[j]]
      output_boards[[j]][which(d == bingo_nums[i], arr.ind = T)] <- 1
      if (any(colSums(output_boards[[j]]) == 5) | any(rowSums(output_boards[[j]]) == 5)){
        return(list(i=i,j=j,output_boards=output_boards))
      }
    }
  }
}

find_sum <- function(input_boards, bingo_nums){
  k <- day_4_1(input_boards)
  bingo_nums[k$i]*sum(input_boards[[k$j]][which(k$output_boards[[k$j]] == 0, arr.ind = T)])
}


#test 
bingo_nums <- c(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)
x1 <- read.delim("2021/Data/4_1_test.txt", header = F, sep = "" )
input_boards <- list()
for (i in 1:(nrow(x1)/5)){
  input_boards[[i]] <- x1[(i*5-4):(i*5),1:5]
}
find_sum(input_boards, bingo_nums)

#answer 
bingo_nums <- c(17,58,52,49,72,33,55,73,27,69,88,80,9,7,59,98,63,42,84,37,87,28,97,66,79,77,61,48,83,5,94,26,70,12,51,82,99,45,22,64,10,78,13,18,15,39,8,30,68,65,40,21,6,86,90,29,60,4,38,3,43,93,44,50,41,96,20,62,19,91,23,36,47,92,76,31,67,11,0,56,95,85,35,16,2,14,75,53,1,57,81,46,71,54,24,74,89,32,25,34)
x1 <- read.delim("2021/Data/4_1.txt", header = F, sep = "" )
input_boards <- list()
for (i in 1:(nrow(x1)/5)){
  input_boards[[i]] <- x1[(i*5-4):(i*5),1:5]
}
find_sum(input_boards, bingo_nums)

#### part 2 ####

day_4_2 <- function(input_boards){
  output_boards <- list()
  for (i in 1:length(input_boards)){
    output_boards[[i]] <- matrix(0, nrow=5, ncol=5)
  }
  
  output_board <- numeric()
  output_nums <- numeric()
  for (i in 1:length(bingo_nums)){
    for (j in 1:length(input_boards)){
      d <- input_boards[[j]]
      output_boards[[j]][which(d == bingo_nums[i], arr.ind = T)] <- 1
      if (any(colSums(output_boards[[j]]) == 5) | any(rowSums(output_boards[[j]]) == 5)){
        output_board <- c(output_board, j)
        output_nums <- c(output_nums, i)
        if (all(1:length(input_boards) %in% output_board)){
          return(list(i=i,j=j,output_boards=output_boards))
        }
      }
    }
  }
}

find_sum2 <- function(input_boards, bingo_nums){
  k <- day_4_2(input_boards)
  bingo_nums[k$i]*sum(input_boards[[k$j]][which(k$output_boards[[k$j]] == 0, arr.ind = T)])
}

#test 
bingo_nums <- c(7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1)
x1 <- read.delim("2021/Data/4_1_test.txt", header = F, sep = "" )
input_boards <- list()
for (i in 1:(nrow(x1)/5)){
  input_boards[[i]] <- x1[(i*5-4):(i*5),1:5]
}
find_sum2(input_boards, bingo_nums)

#answer 
bingo_nums <- c(17,58,52,49,72,33,55,73,27,69,88,80,9,7,59,98,63,42,84,37,87,28,97,66,79,77,61,48,83,5,94,26,70,12,51,82,99,45,22,64,10,78,13,18,15,39,8,30,68,65,40,21,6,86,90,29,60,4,38,3,43,93,44,50,41,96,20,62,19,91,23,36,47,92,76,31,67,11,0,56,95,85,35,16,2,14,75,53,1,57,81,46,71,54,24,74,89,32,25,34)
x1 <- read.delim("2021/Data/4_1.txt", header = F, sep = "" )
input_boards <- list()
for (i in 1:(nrow(x1)/5)){
  input_boards[[i]] <- x1[(i*5-4):(i*5),1:5]
}
find_sum2(input_boards, bingo_nums)

