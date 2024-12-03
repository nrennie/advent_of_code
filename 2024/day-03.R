input <- readLines("2024/data/input.txt")


# Part 1 ------------------------------------------------------------------

start <- "mul\\("
end <- "\\)"
patt <- paste0("(?<=", start, ")(.{0,7}?)(?=", end, ")")

stringr::str_extract_all(input, patt) |>
  unlist() |>
  tibble::as_tibble() |>
  tidyr::separate_wider_delim(value,
    delim = ",", names = c("left", "right"),
    too_many = "drop",
    too_few = "align_start"
  ) |>
  dplyr::mutate(
    dplyr::across(c(left, right), as.numeric)
  ) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    n = left * right
  ) |>
  dplyr::pull(n) |>
  sum() |>
  suppressWarnings()


# Part 2 ------------------------------------------------------------------

start <- "mul\\("
end <- "\\)"
patt <- paste0("(?<=", start, ")(.{0,7}?)(?=", end, ")", "|do\\(\\)|don't\\(\\)")

stringr::str_extract_all(input, patt) |>
  unlist() |>
  tibble::as_tibble() |>
  dplyr::mutate(
    do_calc = dplyr::case_when(
      (dplyr::row_number() == 1) ~ "yes",
      value == "do()" ~ "yes",
      value == "don't()" ~ "no"
    )
  ) |>
  tidyr::fill(
    do_calc,
    .direction = "down"
  ) |>
  dplyr::filter(
    !(value %in% c("do()", "don't()")),
    do_calc == "yes"
  ) |>
  tidyr::separate_wider_delim(value,
    delim = ",", names = c("left", "right"),
    too_many = "drop",
    too_few = "align_start"
  ) |>
  dplyr::mutate(
    dplyr::across(c(left, right), as.numeric)
  ) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    n = left * right
  ) |>
  dplyr::pull(n) |>
  sum() |>
  suppressWarnings()
