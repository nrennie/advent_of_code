input <- readLines("2023/data/input-09.txt")

# Part 1
get_pred <- function(x) {
  x <- x |> 
    stringr::str_split_1(" ") |> 
    as.numeric()
  t <- list(x)
  while (!all(t[[length(t)]] == 0)) {
    new_t <- diff(t[[length(t)]])
    t <- append(t, list(new_t))
  }

  for (i in length(t):1) {
    if (i == length(t)) {
      t[[i]] <- c(t[[i]], 0)
    } else {
      t[[i]] <- c(t[[i]], tail(t[[i]], 1) + tail(t[[i+1]], 1))
    }
  }
  return(tail(t[[1]], 1))
}

purrr::map_vec(.x = input, .f = ~get_pred(.x)) |> 
  sum()

# Part 2
get_hist <- function(x) {
  x <- x |> 
    stringr::str_split_1(" ") |> 
    as.numeric()
  t <- list(x)
  while (!all(t[[length(t)]] == 0)) {
    new_t <- diff(t[[length(t)]])
    t <- append(t, list(new_t))
  }
  
  for (i in length(t):1) {
    if (i == length(t)) {
      t[[i]] <- c(0, t[[i]])
    } else {
      t[[i]] <- c(head(t[[i]], 1) - head(t[[i+1]], 1), t[[i]])
    }
  }
  return(head(t[[1]], 1))
}
purrr::map_vec(.x = input, .f = ~get_hist(.x)) |> 
  sum()
