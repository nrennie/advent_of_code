## Day 6

#### part 1 ####

transform_state <- function(s){
  if (s %in% 1:8){
    return(s-1)
  }
  else if (s == 0)
    return(c(6, 8))
}

day_6_1 <- function(init_state, days){
  for (i in 1:days){
    #first day 
    if (i == 1){
      current <- c()
      for (j in 1:length(init_state)){
        current <- c(current, transform_state(init_state[j]))
      }
    } else{
      previous <- current
      for (j in 1:length(previous)){
        if (j == 1){
          current <- c()
          current <- c(current, transform_state(previous[j]))
        } else{
          current <- c(current, transform_state(previous[j]))
        }
      }
    }
  }
  length(current)
}

#test
init_state <- as.numeric(unname(read.delim("2021/Data/6_1_test.txt", header = F, sep = ",")))
day_6_1(init_state, 80)

#answer
init_state <- as.numeric(unname(read.delim("2021/Data/6_1.txt", header = F, sep = ",")))
day_6_1(init_state, 80)


#### part 2 ####

day_6_2 <- function(init_state, days){
  for (i in 1:days){
    print(i)
    #first day 
    if (i == 1){
      current <- unlist(lapply(init_state, function(x) transform_state(x)))
    } else{
      previous <- current
      current <- unlist(lapply(previous, function(x) transform_state(x)))
    }
  }
  current
}

#test
init_state <- as.numeric(unname(read.delim("2021/Data/6_1_test.txt", header = F, sep = ",")))
day_6_2(init_state, 256)

#answer
init_state <- as.numeric(unname(read.delim("2021/Data/6_1.txt", header = F, sep = ",")))
k <- lapply(0:8, function(x) day_6_2(x, 128))
current_128 <- lengths(k)
day <- unlist(lapply(2:6, function(x) sum(table(k[[x]])*current_128)))
answer <- sum(table(init_state)*day)
format(answer, scientific = FALSE)


