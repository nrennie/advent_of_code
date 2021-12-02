## Day 1

#### part 1 #### 

#x is a vector of depths
day_1_1 <- function(x){
  sum(unlist(lapply(2:length(x), function(i) x[i] > x[i-1])))
}

#test
depths1 <- c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
day_1_1(depths1)

#answer
depths2 <- read.delim("2021/Data/1_1.txt", header = F)[,1]
day_1_1(depths2)


#### part 2 ####
day_1_1(zoo::rollapply(data=depths2, width=3, sum))
