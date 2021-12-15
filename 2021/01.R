## Day 1

#### part 1 #### 
day_1_1 <- function(x){
  sum(unlist(lapply(2:length(x), function(i) x[i] > x[i-1])))
}

#test
depths1 <- read.delim("2021/Data/1_1_test.txt", header = F)[,1]
day_1_1(depths1)

#answer
depths2 <- read.delim("2021/Data/1_1.txt", header = F)[,1]
day_1_1(depths2)


#### part 2 ####
day_1_1(zoo::rollapply(data=depths2, width=3, sum))
