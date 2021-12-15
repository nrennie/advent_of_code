## Day 5

#### part 1 ####
day_5_1 <- function(x1){
  d1 <- data.frame(start_x = as.numeric(substring(x1[,1], 1, regexpr(",",x1[,1])-1)),
                   end_x = as.numeric(substring(x1[,3], 1, regexpr(",",x1[,3])-1)),
                   start_y = as.numeric(substring(x1[,1], regexpr(",",x1[,1])+1, nchar(x1[,1]))),
                   end_y = as.numeric(substring(x1[,3], regexpr(",",x1[,3])+1, nchar(x1[,3]))))
  
  output <- matrix(0, ncol=max(c(d1$start_x, d1$end_x))+1, nrow=max(c(d1$start_y, d1$end_y))+1)
  rownames(output) <- 0:max(c(d1$start_y, d1$end_y))
  colnames(output) <- 0:max(c(d1$start_x, d1$end_x))
  for (i in 1:nrow(d1)){
    if (d1$start_x[i] == d1$end_x[i] | d1$start_y[i] == d1$end_y[i]){
      x <- seq(d1$start_x[i], d1$end_x[i])
      y <- seq(d1$start_y[i], d1$end_y[i])
      d <- expand.grid(x,y)
      for (j in 1:nrow(d)){
        output[as.character(d$Var2[j]), as.character(d$Var1[j])] <- output[as.character(d$Var2[j]), as.character(d$Var1[j])] + 1
      }
    }
  }
  length(which(output >= 2))
}

#test 
x1 <- read.delim("2021/Data/5_1_test.txt", header = F, sep = " ")
day_5_1(x1)

#answer
x1 <- read.delim("2021/Data/5_1.txt", header = F, sep = " ")
day_5_1(x1)

#### part 2 ####

day_5_2 <- function(x1){
  d1 <- data.frame(start_x = as.numeric(substring(x1[,1], 1, regexpr(",",x1[,1])-1)),
                   end_x = as.numeric(substring(x1[,3], 1, regexpr(",",x1[,3])-1)),
                   start_y = as.numeric(substring(x1[,1], regexpr(",",x1[,1])+1, nchar(x1[,1]))),
                   end_y = as.numeric(substring(x1[,3], regexpr(",",x1[,3])+1, nchar(x1[,3]))))
  
  output <- matrix(0, ncol=max(c(d1$start_x, d1$end_x))+1, nrow=max(c(d1$start_y, d1$end_y))+1)
  rownames(output) <- 0:max(c(d1$start_y, d1$end_y))
  colnames(output) <- 0:max(c(d1$start_x, d1$end_x))
  for (i in 1:nrow(d1)){
    #horizontal / vertical lines
    if (d1$start_x[i] == d1$end_x[i] | d1$start_y[i] == d1$end_y[i]){
      x <- seq(d1$start_x[i], d1$end_x[i])
      y <- seq(d1$start_y[i], d1$end_y[i])
      d <- expand.grid(x,y)
      for (j in 1:nrow(d)){
        output[as.character(d$Var2[j]), as.character(d$Var1[j])] <- output[as.character(d$Var2[j]), as.character(d$Var1[j])] + 1
      }
    } else {
      x <- seq(d1$start_x[i], d1$end_x[i])
      y <- seq(d1$start_y[i], d1$end_y[i])
      d <- data.frame(x,y)
      for (j in 1:nrow(d)){
        output[as.character(d$y[j]), as.character(d$x[j])] <- output[as.character(d$y[j]), as.character(d$x[j])] + 1
      }
    }
  }
  length(which(output >= 2))
}

#test 
x1 <- read.delim("2021/Data/5_1_test.txt", header = F, sep = " ")
day_5_2(x1)

#answer
x1 <- read.delim("2021/Data/5_1.txt", header = F, sep = " ")
day_5_2(x1)


