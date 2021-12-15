## Day 2
library(magrittr)

#### part 1 ####
day_2_1 <- function(input){
  d <- tibble::as_tibble(matrix(unlist(stringr::str_split(input, pattern=" ")), byrow=T, ncol=2)) %>%
    dplyr::mutate(V2 = as.numeric(V2))
  horizontal <- dplyr::filter(d, V1 == "forward") %>%
    dplyr::pull(V2) %>%
    sum()
  depth <- dplyr::filter(d, V1 == "down") %>%
    dplyr::pull(V2) %>%
    sum() - 
    dplyr::filter(d, V1 == "up") %>%
    dplyr::pull(V2) %>%
    sum()
  horizontal*depth
}

#test
x1 <- read.delim("2021/Data/2_1_test.txt", header = F)[,1]
day_2_1(x1)

#answer
x2 <- read.delim("2021/Data/2_1.txt", header = F)[,1]
day_2_1(x2)


#### part 2 ####
day_2_2 <- function(input){
  d <- tibble::as_tibble(matrix(unlist(stringr::str_split(input, pattern=" ")), byrow=T, ncol=2)) %>%
    dplyr::mutate(V2 = as.numeric(V2))
  horizontal <- dplyr::filter(d, V1 == "forward") %>%
    dplyr::pull(V2) %>%
    sum()
  aim <- 0
  depth <- 0
  for (i in 1:nrow(d)){
    if (d[i,1] == "down"){
      aim = aim + d[i,2]
    } 
    else if (d[i,1] == "up"){
      aim = aim - d[i,2]
    } 
    else if (d[i,1] == "forward"){
      depth = depth + d[i,2]*aim
    } 
  }
  horizontal*depth
}

#test 
day_2_2(x1)

#answer
day_2_2(x2)






















