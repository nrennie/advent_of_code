input <- readLines("2023/data/input-03.txt")

# Convert input to matrix
input_mat <- input |> 
  tibble::as_tibble() |> 
  dplyr::mutate(value = stringr::str_split(value, pattern = "")) |> 
  dplyr::mutate(column_num = dplyr::row_number()) |> 
  tidyr::unnest(cols = value) |> 
  dplyr::group_by(column_num) |> 
  dplyr::mutate(row_num = dplyr::row_number()) |> 
  dplyr::ungroup() |> 
  tidyr::pivot_wider(names_from = column_num, values_from = value) |> 
  dplyr::select(-row_num) |> 
  as.matrix() |> 
  t()

# Helper functions
is_number <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}
is_dot <- function(x) {
  x == "."
}
is_symbol <- function(x) {
  output <- logical(length = length(x))
  for (i in seq_along(x)) {
    output[i] <- !any(c(is_dot(x[i]), is_number(x[i])))
  }
  output
}
#is_symbol(".")
#is_symbol("3")
#is_symbol("*")
#is_symbol(c(".", "*"))

number_pos <- stringr::str_locate_all(input, pattern = "(\\d{1,})") |> 
  purrr::map(.f = ~as.data.frame(.x)) |> 
  dplyr::bind_rows(.id = "row")
number_pos$number <- NA
number_pos$has_symbol <- NA

# Part 1
for (i in 1:nrow(number_pos)) {
  start <- number_pos$start[i]
  end <- number_pos$end[i]
  row <- as.numeric(number_pos$row[i])
  # fill in number
  number_pos$number[i] <- as.numeric(stringr::str_flatten(input_mat[row, start:end]))
  # check if adjacent symbol
  has_adj <- logical()
  if (row != 1) {
    # check above
    check <- any(is_symbol(input_mat[row - 1, start:end]))
    has_adj <- c(has_adj, check)
    
    if (start != 1)  {
      # check upper left diagonal
      check <- is_symbol(input_mat[row - 1, start - 1])
      has_adj <- c(has_adj, check)
    }
    if (end != ncol(input_mat)) {
      # check upper right diagonal
      check <- is_symbol(input_mat[row - 1, end + 1])
      has_adj <- c(has_adj, check)
    }
  }
  
  if (row != nrow(input_mat)) {
    # check below
    check <- any(is_symbol(input_mat[row + 1, start:end]))
    has_adj <- c(has_adj, check)
    
    if (start != 1)  {
      # check lower left diagonal
      check <- is_symbol(input_mat[row + 1, start - 1])
      has_adj <- c(has_adj, check)
    }
    
    if (end != ncol(input_mat)) {
      # check lower right diagonal
      check <- is_symbol(input_mat[row + 1, end + 1])
      has_adj <- c(has_adj, check)
    }
  }
  
  
  if (start != 1) {
    # check left
    check <- is_symbol(input_mat[row, start - 1])
    has_adj <- c(has_adj, check)
  }
  if (end != ncol(input_mat)) {
    # check right
    check <- is_symbol(input_mat[row, end + 1])
    has_adj <- c(has_adj, check)
  }
  
  number_pos$has_symbol[i] <- any(has_adj)
}

number_pos |> 
  dplyr::filter(has_symbol) |> 
  dplyr::pull(number) |> 
  sum()


# Part 2
gear_pos <- stringr::str_locate_all(input, pattern = "\\*") |> 
  purrr::map(.f = ~as.data.frame(.x)) |> 
  dplyr::bind_rows(.id = "row")
gear_pos$has_number <- NA
gear_pos$gear_ratio <- NA

for (i in 1:nrow(gear_pos)) {
  start <- gear_pos$start[i]
  end <- gear_pos$end[i]
  row <- as.numeric(gear_pos$row[i])
  # check if adjacent symbol
  has_adj <- logical()
  check_up <- FALSE
  check_up_left <- FALSE
  check_up_right <- FALSE
  check_down <- FALSE
  check_down_left <- FALSE
  check_down_right <- FALSE
  check_left <- FALSE
  check_right <- FALSE
  # start checks
  if (row != 1) {
    # check above
    check_up <- is_number(input_mat[row - 1, start])
    if (start != 1 & !check_up)  {
      # check upper left diagonal
      check_up_left <- is_number(input_mat[row - 1, start - 1])
    }
    if (end != ncol(input_mat) & !check_up) {
      # check upper right diagonal
      check_up_right <- is_number(input_mat[row - 1, start + 1])
    }
  }
  
  if (row != nrow(input_mat)) {
    # check below
    check_down <- is_number(input_mat[row + 1, start])
    if (start != 1 & !check_down)  {
      # check lower left diagonal
      check_down_left <- is_number(input_mat[row + 1, start - 1])
    }
    
    if (end != ncol(input_mat) & !check_down) {
      # check lower right diagonal
      check_down_right <- is_number(input_mat[row + 1, start + 1])
    }
  }
  
  if (start != 1) {
    # check left
    check_left <- is_number(input_mat[row, start - 1])
  }
  if (end != ncol(input_mat)) {
    # check right
    check_right <- is_number(input_mat[row, start + 1])
  }
  
  # get all 
  checks <- c(
    check_up, 
    check_up_left, 
    check_up_right, 
    check_down, 
    check_down_left, 
    check_down_right, 
    check_left, 
    check_right)
  is_gear <- (sum(checks) == 2)
  gear_pos$has_number[i] <- is_gear
  
  # find out what numbers are
  if (is_gear) {
    numbers <- numeric()
    # up 
    if (check_up) {
      num <- number_pos |> 
        dplyr::mutate(row = as.numeric(row)) |> 
        dplyr::filter(.data$row == (.env$row - 1),
                      .data$start <= .env$start,
                      .data$end >= .env$start) |> 
        dplyr::pull(number)
      numbers <- c(numbers, num)
    }
    if (check_up_left) {
      num <- number_pos |> 
        dplyr::mutate(row = as.numeric(row)) |> 
        dplyr::filter(.data$row == (.env$row - 1),
                      .data$start <= .env$start - 1,
                      .data$end >= .env$start - 1) |> 
        dplyr::pull(number)
      numbers <- c(numbers, num)
    }
    if (check_up_right) {
      num <- number_pos |> 
        dplyr::mutate(row = as.numeric(row)) |> 
        dplyr::filter(.data$row == (.env$row - 1),
                      .data$start <= .env$start + 1,
                      .data$end >= .env$start + 1) |> 
        dplyr::pull(number)
      numbers <- c(numbers, num)
    }
    # down 
    if (check_down) {
      num <- number_pos |> 
        dplyr::mutate(row = as.numeric(row)) |> 
        dplyr::filter(.data$row == (.env$row + 1),
                      .data$start <= .env$start,
                      .data$end >= .env$start) |> 
        dplyr::pull(number)
      numbers <- c(numbers, num)
    }
    if (check_down_left) {
      num <- number_pos |> 
        dplyr::mutate(row = as.numeric(row)) |> 
        dplyr::filter(.data$row == (.env$row + 1),
                      .data$start <= .env$start - 1,
                      .data$end >= .env$start - 1) |> 
        dplyr::pull(number)
      numbers <- c(numbers, num)
    }
    if (check_down_right) {
      num <- number_pos |> 
        dplyr::mutate(row = as.numeric(row)) |> 
        dplyr::filter(.data$row == (.env$row + 1),
                      .data$start <= .env$start + 1,
                      .data$end >= .env$start + 1) |> 
        dplyr::pull(number)
      numbers <- c(numbers, num)
    }
    # left/right
    if (check_left) {
      num <- number_pos |> 
        dplyr::mutate(row = as.numeric(row)) |> 
        dplyr::filter(.data$row == (.env$row),
                      .data$start <= .env$start - 1,
                      .data$end >= .env$start - 1) |> 
        dplyr::pull(number)
      numbers <- c(numbers, num)
    }
    if (check_right) {
      num <- number_pos |> 
        dplyr::mutate(row = as.numeric(row)) |> 
        dplyr::filter(.data$row == (.env$row),
                      .data$start <= .env$start + 1,
                      .data$end >= .env$start + 1) |> 
        dplyr::pull(number)
      numbers <- c(numbers, num)
    }
    gear_pos$gear_ratio[i] <- prod(numbers)
  }
  
}

gear_pos |> 
  dplyr::filter(!is.na(gear_ratio)) |> 
  dplyr::pull(gear_ratio) |> 
  sum()
