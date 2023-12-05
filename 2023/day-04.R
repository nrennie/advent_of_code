input <- readLines("2023/data/input-04.txt")

# Process data
input_df <- input |> 
  tibble::as_tibble() |> 
  dplyr::mutate(value = stringr::str_remove(value, ".*: ")) |> 
  tidyr::separate_wider_delim(value,
                              names = c("winning", "card"),
                              delim = " | ") |> 
  dplyr::mutate(winning = purrr::map(
    .x = stringr::str_split(winning, " "),
    .f = ~na.omit(as.numeric(.x))
  )) |> 
  dplyr::mutate(card = purrr::map(
    .x = stringr::str_split(card, " "),
    .f = ~na.omit(as.numeric(.x))
  ))

# Part 1
winning <- numeric(length = nrow(input_df))
for (i in 1:nrow(input_df)) {
  winners <- input_df$card[i][[1]][which(input_df$card[i][[1]] %in% input_df$winning[i][[1]])]
  if (length(winners) > 0) {
    winning[i] <- 2^(length(winners) - 1)
  } else {
    winning[i] <- 0
  }
}
sum(winning)

# Part 2
card_counts <- rep(1, length = nrow(input_df))
for (i in 1:nrow(input_df)) {
  num_winners <- length(which(input_df$card[i][[1]] %in% input_df$winning[i][[1]]))
  for (j in seq_len(num_winners)) {
    card_counts[i + j] <- card_counts[i + j] + card_counts[i]
  }
}
sum(card_counts)
