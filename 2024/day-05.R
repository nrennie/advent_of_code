input <- readLines("2024/data/input.txt")

input_rules <- input[1:(which(input == "")-1)]
input_tests <- input[(which(input == "")+1):length(input)]


# Part 1 ------------------------------------------------------------------

rules <- input_rules |> 
  tibble::as_tibble() |> 
  tidyr::separate_wider_delim(
    value,
    delim = "|",
    names = c("before", "after")
  ) |> 
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(), as.numeric
    )
  )

tests <- input_tests |> 
  as.list() |> 
  purrr::map(stringr::str_split_1, pattern = ",") |> 
  purrr::map(as.numeric)

check_test <- function(x) {
  valid_rules <- rules |> 
    dplyr::filter(
      before %in% x, after %in% x
    )
  for (i in 1:nrow(valid_rules)) {
    pos1 <- which(x == valid_rules$before[i])
    pos2 <- which(x == valid_rules$after[i])
    if (pos1 > pos2) {
      return(FALSE)
    }
  }
  return(TRUE)
}

mid_item <- function(x) {
  x[ceiling((length(x)/2))]
}

check_all_tests <- purrr::map(
  tests, check_test
)

mid_values <- purrr::map(
  tests, mid_item
)

sum(unlist(mid_values) * unlist(check_all_tests))


# Part 2 ------------------------------------------------------------------

which_incorrect <- which(!unlist(check_all_tests))
incorrect_tests <- tests[which_incorrect]

sort_test <- function(x) {
  valid_rules <- rules |> 
    dplyr::filter(
      before %in% x, after %in% x
    )
  new_x <- valid_rules |> 
    dplyr::group_by(before) |> 
    dplyr::summarize(after = paste0(after, collapse = ","), .groups = "drop") |> 
    dplyr::mutate(
      vec_len = stringr::str_count(after, ",")
    ) |> 
    dplyr::arrange(-vec_len)
  new_x <- as.numeric(c(new_x$before, new_x$after[nrow(new_x)]))
  return(mid_item(new_x))
}

purrr::map(
  incorrect_tests,
  sort_test
) |> 
  unlist() |> 
  sum()
