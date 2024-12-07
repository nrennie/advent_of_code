input <- readLines("2024/data/input.txt")


# Part 1 ------------------------------------------------------------------

# forwards / backwards
x1 <- sum(stringr::str_count(input, "XMAS"))
x2 <- sum(stringr::str_count(input, "SAMX"))

# up / down
ud_input <- input |> 
  stringr::str_split("") |> 
  unlist() |> 
  matrix(ncol = nchar(input[1]), byrow = TRUE) |> 
  t() |> 
  as.data.frame() |> 
  tibble::as_tibble() |> 
  tidyr::unite(col = "word", sep = "") |> 
  dplyr::pull(word)
x3 <- sum(stringr::str_count(ud_input, "XMAS"))
x4 <- sum(stringr::str_count(ud_input, "SAMX"))

# diagonal topleft-bottomright
get_all_diagonals <- function(mat) {
  rows <- nrow(mat)
  cols <- ncol(mat)
  diagonals <- list()
  for (k in 0:(cols - 1)) {
    diagonals[[length(diagonals) + 1]] <- mat[row(mat) == col(mat) - k]
  }
  for (k in 1:(rows - 1)) {
    diagonals[[length(diagonals) + 1]] <- mat[row(mat) == col(mat) + k]
  }
  return(diagonals)
}

d1_input <- input |> 
  stringr::str_split("") |> 
  unlist() |> 
  matrix(ncol = nchar(input[1]), byrow = TRUE) |> 
  get_all_diagonals() |> 
  purrr::map(stringr::str_flatten) |> 
  unlist()
x5 <- sum(stringr::str_count(d1_input, "XMAS"))
x6 <- sum(stringr::str_count(d1_input, "SAMX"))

# diagonal topright-bottomleft

d2_input <- input |> 
  stringr::str_split("") |> 
  unlist() |> 
  matrix(ncol = nchar(input[1]), byrow = TRUE) |> 
  _[ ,nchar(input[1]):1] |> 
  get_all_diagonals() |> 
  purrr::map(stringr::str_flatten) |> 
  unlist()
x7 <- sum(stringr::str_count(d2_input, "XMAS"))
x8 <- sum(stringr::str_count(d2_input, "SAMX"))

# sum
x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8


# Part 2 ------------------------------------------------------------------

input_mat <- input |> 
  stringr::str_split("") |> 
  unlist() |> 
  matrix(ncol = nchar(input[1]), byrow = TRUE)

A_loc <- which(input_mat == "A", arr.ind = TRUE) |> 
  tibble::as_tibble() |> 
  dplyr::filter(
    !(row %in% c(1, nchar(input[1]))),
    !(col %in% c(1, nchar(input[1])))
  )

xmas <- 0
for (a in 1:nrow(A_loc)) {
  x1 <- FALSE
  x2 <- FALSE
  x3 <- FALSE
  x4 <- FALSE
  # tl-br
  if (input_mat[A_loc$row[a]-1, A_loc$col[a]-1] == "M" & input_mat[A_loc$row[a]+1, A_loc$col[a]+1] == "S") {
    x1 <- TRUE
  }
  if (input_mat[A_loc$row[a]-1, A_loc$col[a]-1] == "S" & input_mat[A_loc$row[a]+1, A_loc$col[a]+1] == "M") {
    x2 <- TRUE
  }
  # tr-bl
  if (input_mat[A_loc$row[a]-1, A_loc$col[a]+1] == "M" & input_mat[A_loc$row[a]+1, A_loc$col[a]-1] == "S") {
    x3 <- TRUE
  }
  if (input_mat[A_loc$row[a]-1, A_loc$col[a]+1] == "S" & input_mat[A_loc$row[a]+1, A_loc$col[a]-1] == "M") {
    x4 <- TRUE
  }
  
  if ((x1 | x2) & (x3 | x4)) {
    xmas <- xmas + 1
  }
}
xmas

