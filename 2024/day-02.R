input <- readLines("2024/data/input.txt")


# Part 1 ------------------------------------------------------------------

# Function to process one string
part1 <- function(x) {
  x <- as.numeric(stringr::str_split_1(x, " "))
  diff_x <- diff(x)
  if (all(diff_x < 0) | all(diff_x > 0)) {
    if (all(abs(diff_x) <= 3)) {
      return(TRUE) 
    } else {
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

# Map over input
purrr::map(
  .x = input,
  .f = ~part1(.x)
) |> 
  unlist() |> 
  sum()


# Part 2 ------------------------------------------------------------------

# Function to check removing one
part2 <- function(x) {
  check_init <- part1(x)
  if (check_init) {
    return(TRUE)
  } else {
    x <- as.numeric(stringr::str_split_1(x, " "))
    for (i in 1:length(x)) {
      new_x <- stringr::str_flatten(x[-i], " ")
      check_i <- part1(new_x)
      if (check_i) {
        return(TRUE)
      }
    }
  }
}

# Map over input
purrr::map(
  .x = input,
  .f = ~part2(.x)
) |> 
  unlist() |> 
  sum()
