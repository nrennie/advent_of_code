input <- readLines("2023/data/input-02.txt")

# Part 1
max_vals <- data.frame(
  max_num = c(12, 13, 14),
  colour = c("red", "green", "blue")
)
input |> 
  tibble::as_tibble() |> 
  tidyr::separate_wider_delim(value, names = c("game", "results"), delim = ": ") |> 
  tidyr::separate_wider_delim(results, delim = "; ", names_sep = "", too_few = "align_start") |> 
  tidyr::separate_wider_delim(starts_with("results"), delim = ", ", names_sep = "_", too_few = "align_start") |> 
  tidyr::pivot_longer(-game, names_to = "results", values_to = "result") |> 
  dplyr::select(-results) |> 
  dplyr::mutate(game = as.numeric(stringr::str_remove(game, "Game "))) |> 
  tidyr::drop_na() |> 
  tidyr::separate_wider_delim(result, names = c("number", "colour"), delim = " ") |> 
  dplyr::mutate(number = as.numeric(number)) |> 
  dplyr::left_join(max_vals, by = "colour") |> 
  dplyr::mutate(possible = number <= max_num) |> 
  dplyr::group_by(game) |> 
  dplyr::summarise(all_possible = all(possible)) |> 
  dplyr::filter(all_possible) |> 
  dplyr::pull(game) |> 
  sum()

# Part 2
input |> 
  tibble::as_tibble() |> 
  tidyr::separate_wider_delim(value, names = c("game", "results"), delim = ": ") |> 
  tidyr::separate_wider_delim(results, delim = "; ", names_sep = "", too_few = "align_start") |> 
  tidyr::separate_wider_delim(starts_with("results"), delim = ", ", names_sep = "_", too_few = "align_start") |> 
  tidyr::pivot_longer(-game, names_to = "results", values_to = "result") |> 
  dplyr::select(-results) |> 
  dplyr::mutate(game = as.numeric(stringr::str_remove(game, "Game "))) |> 
  tidyr::drop_na() |> 
  tidyr::separate_wider_delim(result, names = c("number", "colour"), delim = " ") |> 
  dplyr::mutate(number = as.numeric(number)) |> 
  dplyr::group_by(game, colour) |> 
  dplyr::summarise(n = max(number)) |> 
  dplyr::ungroup() |> 
  dplyr::group_by(game) |> 
  dplyr::summarise(power = prod(n)) |> 
  dplyr::pull(power) |> 
  sum()
