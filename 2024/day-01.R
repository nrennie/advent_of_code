input <- readLines("2024/data/input.txt")


# Process data ------------------------------------------------------------

input_df <- input |>
  tibble::as_tibble() |>
  tidyr::separate_wider_delim(
    cols = value,
    names = c("left", "right"),
    delim = "   "
  ) |> 
  dplyr::mutate(
    dplyr::across(c(left, right), as.numeric)
  )


# Part 1 ------------------------------------------------------------------

sum(abs(sort(input_df$left) - sort(input_df$right)))


# Part 2 ------------------------------------------------------------------

table(input_df$right) |> 
  tibble::enframe() |> 
  dplyr::mutate(
    name = as.numeric(name)
  ) |> 
  dplyr::right_join(
    dplyr::select(input_df, left), 
    by = c("name" = "left")
  ) |> 
  dplyr::mutate(
    value = as.numeric(value),
    n = name * value
  ) |> 
  dplyr::pull(n) |> 
  sum(na.rm = TRUE)


