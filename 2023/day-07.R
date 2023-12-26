input <- readLines("2023/data/input-07.txt")

input_df <- input |> 
  tibble::as_tibble() |> 
  tidyr::separate_wider_delim(value, names = c("hand", "score"), delim = " ")


# Part 1 ------------------------------------------------------------------

# hand ranks
type_ranks <- tibble::tibble(
  type = c("five of a kind", "four of a kind", "full house",
           "three of a kind", "two pair", "one pair", "high card"),
  rank = 7:1
)
card_order <- c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")

# Function to define type
get_type <- function(x) {
  z <- stringr::str_split_1(x, "")
  tz <- table(z)
  if (max(tz) == 5) {
    type <- "five of a kind"
  } else if (max(tz) == 4){
    type <- "four of a kind"
  } else if (all(c(3, 2) %in% tz)) {
    type <- "full house"
  } else if (max(tz) == 3) {
    type <- "three of a kind"
  } else if (length(which(tz == 2)) == 2) {
    type <- "two pair"
  } else if (max(tz) == 2) {
    type <- "one pair"
  } else {
    type <- "high card"
  }
  return(type)
}

to_number <- function(x) {
  k <- stringr::str_split_1(x, "") |> 
    factor(levels = card_order) |> 
    as.numeric()
  stringr::str_flatten(LETTERS[k])
}

input_df |> 
  dplyr::mutate(type = purrr::map_vec(.x = hand, .f = ~ get_type(.x))) |> 
  dplyr::mutate(score = as.numeric(score)) |> 
  dplyr::left_join(type_ranks, by = "type") |> 
  dplyr::group_by(type) |> 
  dplyr::arrange(rank) |> 
  dplyr::mutate(grp_rank = rank(dplyr::desc(purrr::map_vec(hand, .f = ~to_number(.x))))) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(rank, grp_rank) |> 
  dplyr::mutate(final_rank = dplyr::row_number()) |> 
  dplyr::mutate(output = final_rank * score) |> 
  dplyr::pull(output) |> 
  sum()


# Part 2 ------------------------------------------------------------------

# get joker type
get_type <- function(x) {
  z <- stringr::str_split_1(x, "")
  tz <- table(z)
  if ("J" %in% names(tz)) {
    num_jokers <- tz[["J"]]
  } else {
    num_jokers <- 0
  }
  # if no jokers, do as part 1
  if (num_jokers == 0) {
    if (max(tz) == 5) {
      type <- "five of a kind"
    } else if (max(tz) == 4){
      type <- "four of a kind"
    } else if (all(c(3, 2) %in% tz)) {
      type <- "full house"
    } else if (max(tz) == 3) {
      type <- "three of a kind"
    } else if (length(which(tz == 2)) == 2) {
      type <- "two pair"
    } else if (max(tz) == 2) {
      type <- "one pair"
    } else {
      type <- "high card"
    }
  }
  # if there are jokers, amend
  else {
    # remove jokers from tz
    if (num_jokers != 5) {
      tz <- tz[-which(names(tz) == "J")]
    }
    if (max(tz) == 4 & num_jokers == 1 | 
        max(tz) == 3 & num_jokers == 2 | 
        max(tz) == 2 & num_jokers == 3 | 
        max(tz) == 1 & num_jokers == 4 | 
        num_jokers == 5) {
      type <- "five of a kind"
    } else if (max(tz) == 3 & num_jokers == 1 | 
               max(tz) == 2 & num_jokers == 2 | 
               max(tz) == 1 & num_jokers == 3 |
               num_jokers == 4){
      type <- "four of a kind"
    } else if (length(which(tz == 2)) == 2 & num_jokers == 1) { 
      type <- "full house"
    } else if (max(tz) == 2 & num_jokers == 1 | 
               max(tz) == 1 & num_jokers == 2 |
               num_jokers == 3) {
      type <- "three of a kind"
    } else if (max(tz) == 2 & num_jokers == 1 |
               num_jokers == 2 ) {
      type <- "two pair"
    } else {
      type <- "one pair"
    } 
  }
  return(type)
}

card_order <- c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J")

to_number <- function(x) {
  k <- stringr::str_split_1(x, "") |> 
    factor(levels = card_order) |> 
    as.numeric()
  stringr::str_flatten(LETTERS[k])
}

input_df |> 
  dplyr::mutate(type = purrr::map_vec(.x = hand, .f = ~ get_type(.x))) |> 
  dplyr::mutate(score = as.numeric(score)) |> 
  dplyr::left_join(type_ranks, by = "type") |> 
  dplyr::group_by(type) |> 
  dplyr::arrange(rank) |> 
  dplyr::mutate(grp_rank = rank(dplyr::desc(purrr::map_vec(hand, .f = ~to_number(.x))))) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(rank, grp_rank) |> 
  dplyr::mutate(final_rank = dplyr::row_number()) |> 
  dplyr::mutate(output = final_rank * score) |> 
  dplyr::pull(output) |> 
  sum()