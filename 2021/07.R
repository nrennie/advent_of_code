## Day 7

#### part 1 ####
calc_fuel <- function(x, shift_to){
  sum(abs(x - shift_to))
}

day_7_1 <- function(x){
  m <- median(x)
  calc_fuel(x, m)
}

#test 
x1 <- as.numeric(unname(read.delim("2021/Data/7_1_test.txt", header = F, sep = ",")))
day_7_1(x1)

#answer
x2 <- as.numeric(unname(read.delim("2021/Data/7_1.txt", header = F, sep = ",")))
day_7_1(x2)

#### part 2 ####
calc_fuel2 <- function(x, shift_to){
  steps <- abs(x - shift_to)
  sum(unlist(lapply(steps, function(n) (n*(n+1))/2)))
}
calc_fuel2(x1, 5)

day_7_2 <- function(x){
  m <- median(x)
  val_check <- seq(min(x), max(x))
  output <- unlist(lapply(val_check, function(i) calc_fuel2(x, i)))
  output[which.min(output)]
}

#test
day_7_2(x1)

#answer
day_7_2(x2)







