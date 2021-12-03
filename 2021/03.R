## Day 3

#### Part 1 ####

day_3_1 <- function(input_matrix){
  d <- colSums(input_matrix) > 0.5*nrow(input_matrix)
  epsilon <- strtoi(paste(as.numeric(d), sep="", collapse=""), base = 2)
  gamma <- strtoi(paste(as.numeric(!d), sep="", collapse=""), base = 2)
  epsilon*gamma
}

#test
x1 <- matrix(c(0,0,1,0,0,
              1,1,1,1,0,
              1,0,1,1,0,
              1,0,1,1,1,
              1,0,1,0,1,
              0,1,1,1,1,
              0,0,1,1,1,
              1,1,1,0,0,
              1,0,0,0,0,
              1,1,0,0,1,
              0,0,0,1,0,
              0,1,0,1,0), ncol=5, byrow=T)
day_3_1(x1)            
             
#answer
x2_raw <- read.delim("2021/Data/3_1.txt", header = F, colClasses="character")[,1]
x2 <- matrix(as.numeric(unlist(strsplit(x2_raw, split=""))), byrow=T, ncol=nchar(x2_raw[1]))
day_3_1(x2)            


#### Part 2 ####

library(dplyr)
day_3_2 <- function(input_matrix){
  colnames(input_matrix) <- 1:ncol(input_matrix)
  input <- tibble::as_tibble(input_matrix)
  oxygen <- input
  co2 <- input
  for (i in 1:(ncol(input))){
    if(nrow(co2) > 1){
      co2 <- co2 %>% filter(.[[i]] == as.numeric(sum(co2[,i]) < 0.5*nrow(co2)))
    }
    if(nrow(oxygen)>1){
      oxygen <- oxygen %>% filter(.[[i]] == as.numeric(sum(oxygen[,i]) >= 0.5*nrow(oxygen)))
    }
  }
  strtoi(paste(oxygen, sep="", collapse=""), base = 2)*strtoi(paste(co2, sep="", collapse=""), base = 2)
}

#test
day_3_2(x1)

#answer
day_3_2(x2)

