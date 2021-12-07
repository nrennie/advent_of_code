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
    print(i)
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
init_state <- c(3,4,3,1,2)
day_6_1(init_state, 80)

#answer
init_state <- c(1,1,3,5,1,1,1,4,1,5,1,1,1,1,1,1,1,3,1,1,1,1,2,5,1,1,1,1,1,2,1,4,1,4,1,1,1,1,1,3,1,1,5,1,1,1,4,1,1,1,4,1,1,3,5,1,1,1,1,4,1,5,4,1,1,2,3,2,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,1,1,1,1,1,1,1,2,2,1,1,1,1,1,5,1,1,1,3,4,1,1,1,1,3,1,1,1,1,1,4,1,1,3,1,1,3,1,1,1,1,1,3,1,5,2,3,1,2,3,1,1,2,1,2,4,5,1,5,1,4,1,1,1,1,2,1,5,1,1,1,1,1,5,1,1,3,1,1,1,1,1,1,4,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,3,2,1,1,1,1,2,2,1,2,1,1,1,5,5,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,4,2,1,4,1,1,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,5,1,1,1,1,1,1,1,1,3,1,1,3,3,1,1,1,3,5,1,1,4,1,1,1,1,1,4,1,1,3,1,1,1,1,1,1,1,1,2,1,5,1,1,1,1,1,1,1,1,1,1,4,1,1,1,1)
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
init_state <- c(3,4,3,1,2)
day_6_2(init_state, 256)

#answer
init_state <- c(1,1,3,5,1,1,1,4,1,5,1,1,1,1,1,1,1,3,1,1,1,1,2,5,1,1,1,1,1,2,1,4,1,4,1,1,1,1,1,3,1,1,5,1,1,1,4,1,1,1,4,1,1,3,5,1,1,1,1,4,1,5,4,1,1,2,3,2,1,1,1,1,1,1,1,1,1,1,1,1,1,5,1,1,1,1,1,1,1,1,2,2,1,1,1,1,1,5,1,1,1,3,4,1,1,1,1,3,1,1,1,1,1,4,1,1,3,1,1,3,1,1,1,1,1,3,1,5,2,3,1,2,3,1,1,2,1,2,4,5,1,5,1,4,1,1,1,1,2,1,5,1,1,1,1,1,5,1,1,3,1,1,1,1,1,1,4,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,3,2,1,1,1,1,2,2,1,2,1,1,1,5,5,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,1,4,2,1,4,1,1,1,1,1,1,1,2,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,1,5,1,1,1,1,1,1,1,1,3,1,1,3,3,1,1,1,3,5,1,1,4,1,1,1,1,1,4,1,1,3,1,1,1,1,1,1,1,1,2,1,5,1,1,1,1,1,1,1,1,1,1,4,1,1,1,1)

#run for 128 days for each starting state
current_128_0 <- day_6_2(0, 128)
current_128_1 <- day_6_2(1, 128)
current_128_2 <- day_6_2(2, 128)
current_128_3 <- day_6_2(3, 128)
current_128_4 <- day_6_2(4, 128)
current_128_5 <- day_6_2(5, 128)
current_128_6 <- day_6_2(6, 128)
current_128_7 <- day_6_2(7, 128)
current_128_8 <- day_6_2(8, 128)


current_128 <- c(length(current_128_0), length(current_128_1), length(current_128_2), length(current_128_3), 
                 length(current_128_4), length(current_128_5), length(current_128_6), length(current_128_7),
                 length(current_128_8))

day1 <- sum(table(current_128_1)*current_128)
day2 <- sum(table(current_128_2)*current_128)
day3 <- sum(table(current_128_3)*current_128)
day4 <- sum(table(current_128_4)*current_128)
day5 <- sum(table(current_128_5)*current_128)

day <- c(day1, day2, day3, day4, day5)

#multiple by number of each state
k <- sum(table(init_state)*day)
format(k, scientific = FALSE)
